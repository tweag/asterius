module NPM.Parcel
  ( parcel
  ) where

import qualified Paths_npm_utils
import System.FilePath
import System.IO.Unsafe

{-# NOINLINE parcel #-}
parcel :: FilePath
parcel =
  unsafePerformIO $ do
    datadir <- Paths_npm_utils.getDataDir
    pure $ datadir </> "node_modules" </> ".bin" </> "parcel"
