{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

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
import Asterius.Types
import qualified Data.HashSet as HS
import Language.Haskell.GHC.Toolkit.Run
import System.FilePath
import UnliftIO

data LinkStartException = ChaseException
  { unseen, unavailable :: HS.HashSet AsteriusEntitySymbol
  } deriving (Show)

instance Exception LinkStartException

data LinkStart = LinkStart
  { asteriusLibDir :: FilePath
  , runConfig :: Config
  , targets :: [String]
  , rootSymbols :: [AsteriusEntitySymbol]
  }

defaultLinkStart :: LinkStart
defaultLinkStart =
  LinkStart
    { asteriusLibDir = dataDir </> ".boot" </> "asterius_lib"
    , runConfig = defaultConfig
    , targets = []
    , rootSymbols = []
    }

linkStart :: MonadIO m => LinkStart -> m AsteriusModule
linkStart = undefined
