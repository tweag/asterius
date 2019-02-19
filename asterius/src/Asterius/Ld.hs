{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Ld
  ( LinkTask(..)
  , loadTheWorld
  ) where

import Asterius.Ar
import Asterius.Builtins
import Asterius.Store
import Asterius.Types
import Data.Binary
import Data.Either
import Data.Foldable
import Data.Traversable

data LinkTask = LinkTask
  { linkOutput :: FilePath
  , linkObjs, linkLibs :: [FilePath]
  } deriving (Show)

loadTheWorld :: BuiltinsOptions -> LinkTask -> IO AsteriusStore
loadTheWorld builtins_opts LinkTask {..} = do
  lib <- mconcat <$> for linkLibs loadAr
  objs <- rights <$> for linkObjs decodeFileOrFail
  let builtins_store = builtinsStore builtins_opts
  pure $
    builtins_store <>
    foldl' (\s m -> addModule (currentModuleSymbol m) m s) lib objs
