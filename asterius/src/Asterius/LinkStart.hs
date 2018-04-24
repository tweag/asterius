{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Asterius.LinkStart
  ( LinkStartException(..)
  , LinkStart
  , asteriusLibDir
  , runConfig
  , targets
  , rootSymbols
  , defaultLinkStart
  , linkStart
  ) where

import Asterius.BuildInfo
import Asterius.CodeGen
import qualified Data.ByteString as BS
import Data.Either
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import Data.Serialize
import Data.Traversable
import DynFlags
import Language.Haskell.GHC.Toolkit.Run
import Asterius.Types
import System.FilePath
import UnliftIO

data LinkStartException = ChaseException
  { unseen, unavailable :: HS.HashSet UnresolvedSymbol
  } deriving (Show)

instance Exception LinkStartException

data LinkStart = LinkStart
  { asteriusLibDir :: FilePath
  , runConfig :: Config
  , targets :: [String]
  , rootSymbols :: [UnresolvedSymbol]
  }

defaultLinkStart :: LinkStart
defaultLinkStart =
  LinkStart
    { asteriusLibDir = dataDir </> ".boot" </> "asterius_lib"
    , runConfig = defaultConfig
    , targets = []
    , rootSymbols = []
    }

readSymbolDB :: MonadIO m => LinkStart -> m AsteriusSymbolDB
readSymbolDB LinkStart {..} =
  liftIO $ do
    r <- fmap decode $ BS.readFile $ asteriusLibDir </> ".asterius_sym_db"
    case r of
      Left err -> throwString err
      Right sym_db -> pure sym_db

chase ::
     AsteriusSymbolDB
  -> [UnresolvedSymbol]
  -> Either LinkStartException AsteriusSymbolDB
chase sym_db syms =
  if HS.null final_weeds && all symbolAvailable final_seeds
    then Right AsteriusSymbolDB {symbolMap = final_seeds}
    else Left
           ChaseException
             { unseen = final_weeds
             , unavailable =
                 HS.fromList
                   [ k
                   | (k, AsteriusSymbolInfo {..}) <- HM.toList final_seeds
                   , not symbolAvailable
                   ]
             }
  where
    f (weeds, seeds) =
      let (l, r) =
            partitionEithers
              [ case HM.lookup new_seed $ symbolMap sym_db of
                Just info -> Right (new_seed, info)
                _ -> Left new_seed
              | new_seed <-
                  HS.toList $
                  mconcat
                    [ symbolDirectDeps
                    | (_, AsteriusSymbolInfo {..}) <- HM.toList seeds
                    ]
              ]
          new_weeds = HS.fromList l <> weeds
          new_seeds = HM.fromList r <> seeds
       in if HM.size seeds == HM.size new_seeds
            then (new_weeds, new_seeds)
            else f (new_weeds, new_seeds)
    init_t =
      let (l, r) =
            partitionEithers
              [ case HM.lookup sym $ symbolMap sym_db of
                Just info -> Right (sym, info)
                _ -> Left sym
              | sym <- syms
              ]
       in (HS.fromList l, HM.fromList r)
    (final_weeds', final_seeds) = f init_t
    final_weeds = final_weeds' `HS.difference` ["newCAF"]

linkStart :: MonadIO m => LinkStart -> m AsteriusModule
linkStart ls@LinkStart {..} =
  liftIO $ do
    irs <- runHaskell runConfig targets
    ms <-
      for (M.toList irs) $ \(k, ir) ->
        (marshalToModuleSymbol k, ) <$> marshalHaskellIR unsafeGlobalDynFlags ir
    init_sym_db <-
      (mconcat [moduleSymbolDB k m | (k, m) <- ms] <>) <$> readSymbolDB ls
    case chase init_sym_db rootSymbols of
      Left err -> throwIO err
      Right sym_db -> do
        ps <-
          for (HM.elems $ symbolMap sym_db) $ \AsteriusSymbolInfo {..} ->
            moduleSymbolPath asteriusLibDir symbolSource "asterius_o"
        m <-
          fmap mconcat $
          for (HS.toList $ HS.fromList ps) $ \p -> do
            buf <- BS.readFile p
            case decode buf of
              Left err -> throwString err
              Right m -> pure m
        pure
          AsteriusModule
            { staticsMap =
                HM.filterWithKey (\k _ -> HM.member k $ symbolMap sym_db) $
                staticsMap m
            , staticsErrorMap = mempty
            , functionMap =
                HM.filterWithKey (\k _ -> HM.member k $ symbolMap sym_db) $
                Asterius.Types.functionMap m
            , functionErrorMap = mempty
            }
