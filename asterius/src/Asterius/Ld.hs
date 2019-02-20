{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Ld
  ( LinkTask(..)
  , loadTheWorld
  ) where

import Asterius.Ar
import Asterius.Builtins
import Asterius.Internals
import Asterius.Store
import Asterius.Types
import Data.Either
import Data.Foldable
import Data.Traversable
import Prelude hiding (IO)

data LinkTask = LinkTask
  { linkOutput :: FilePath
  , linkObjs, linkLibs :: [FilePath]
  } deriving (Read, Show)

loadTheWorld :: BuiltinsOptions -> LinkTask -> IO AsteriusStore
loadTheWorld builtins_opts LinkTask {..} = do
  lib <- mconcat <$> for linkLibs loadAr
  objrs <- for linkObjs tryDecodeFile
  let objs = rights objrs
      builtins_store = builtinsStore builtins_opts
  pure $
    builtins_store <>
    foldl' (\s m -> addModule (currentModuleSymbol m) m s) lib objs
