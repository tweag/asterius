{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Store
  ( decodeStore
  , encodeAsteriusModule
  , encodeStore
  , builtinsStore
  , registerModule
  , addModule
  ) where

import Asterius.Builtins
import Asterius.Internals
import Asterius.Types
import qualified Data.HashMap.Lazy as LHM
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Prelude hiding (IO)
import System.Directory
import System.FilePath
import System.IO.Unsafe

asteriusModulePath :: FilePath -> AsteriusModuleSymbol -> FilePath
asteriusModulePath obj_topdir AsteriusModuleSymbol {..} =
  foldr1
    (</>)
    (obj_topdir :
     c8SBS unitId : [c8SBS mod_chunk | mod_chunk <- V.toList moduleName]) <.>
  "asterius_o"

decodeAsteriusModule :: FilePath -> AsteriusModuleSymbol -> IO AsteriusModule
decodeAsteriusModule obj_topdir mod_sym =
  decodeFile $ asteriusModulePath obj_topdir mod_sym

{-# INLINEABLE decodeStore #-}
decodeStore :: FilePath -> IO AsteriusStore
decodeStore store_path = do
  sym_map <- decodeFile store_path
  let obj_topdir = takeDirectory store_path
      mod_syms = HS.toList $ HS.fromList $ HM.elems sym_map
  pure
    AsteriusStore
      { symbolMap = sym_map
      , moduleMap =
          LHM.fromList
            [ ( mod_sym
              , unsafePerformIO $ decodeAsteriusModule obj_topdir mod_sym)
            | mod_sym <- mod_syms
            ]
      }

{-# INLINEABLE encodeAsteriusModule #-}
encodeAsteriusModule ::
     FilePath -> AsteriusModuleSymbol -> AsteriusModule -> IO ()
encodeAsteriusModule obj_topdir mod_sym m = do
  createDirectoryIfMissing True $ takeDirectory obj_path
  encodeFile obj_path m
  where
    obj_path = asteriusModulePath obj_topdir mod_sym

{-# INLINEABLE encodeStore #-}
encodeStore :: FilePath -> AsteriusStore -> IO ()
encodeStore store_path AsteriusStore {..} = do
  createDirectoryIfMissing True $ takeDirectory store_path
  encodeFile store_path symbolMap

{-# INLINEABLE builtinsStore #-}
builtinsStore :: BuiltinsOptions -> AsteriusStore
builtinsStore opts =
  addModule
    rtsAsteriusModuleSymbol
    (rtsAsteriusModule opts)
    AsteriusStore {symbolMap = mempty, moduleMap = mempty}

{-# INLINEABLE registerModule #-}
registerModule ::
     FilePath
  -> AsteriusModuleSymbol
  -> AsteriusModule
  -> AsteriusStore
  -> AsteriusStore
registerModule obj_topdir mod_sym AsteriusModule {..} AsteriusStore {..} =
  AsteriusStore
    { symbolMap =
        HM.unions
          [ f staticsMap
          , f staticsErrorMap
          , f functionMap
          , f functionErrorMap
          , symbolMap
          ]
    , moduleMap =
        LHM.insert
          mod_sym
          (unsafePerformIO $ decodeAsteriusModule obj_topdir mod_sym)
          moduleMap
    }
  where
    f = fmap (const mod_sym)

{-# INLINEABLE addModule #-}
addModule ::
     AsteriusModuleSymbol -> AsteriusModule -> AsteriusStore -> AsteriusStore
addModule mod_sym m@AsteriusModule {..} AsteriusStore {..} =
  AsteriusStore
    { symbolMap =
        HM.unions
          [ f staticsMap
          , f staticsErrorMap
          , f functionMap
          , f functionErrorMap
          , symbolMap
          ]
    , moduleMap = LHM.insert mod_sym m moduleMap
    }
  where
    f = fmap (const mod_sym)
