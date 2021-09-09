{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.JSGen.Bundle where

import Asterius.Internals.Temp
import Asterius.Sysroot
import qualified Data.ByteString as BS
import Data.Foldable
import System.Directory
import System.FilePath
import System.Process

data BundleTask = BundleTask
  { entry :: FilePath,
    outputPath :: FilePath,
    outputFileName :: FilePath,
    outputLibrary :: Maybe String
  }

bundle :: BundleTask -> IO ()
bundle BundleTask {..} =
  callProcess "npx" $
    [ "webpack",
      "--mode",
      "production",
      "--entry",
      entry,
      "--output-path",
      outputPath,
      "--output-filename",
      outputFileName
    ]
      <> ( case outputLibrary of
             Just lib -> ["--output-library", lib]
             _ -> []
         )

bundleRTS :: IO BS.ByteString
bundleRTS = withTempDir "asterius" $ \tmpdir -> do
  let rts_dir = dataDir </> "rts"
      rts_browser_dir = rts_dir </> "browser"
  rts_mjs <- filter ((== ".mjs") . takeExtension) <$> listDirectory rts_dir
  for_ rts_mjs $ \mjs -> copyFile (rts_dir </> mjs) (tmpdir </> mjs)
  rts_browser_mjs <- listDirectory rts_browser_dir
  for_ rts_browser_mjs $
    \mjs -> copyFile (rts_browser_dir </> mjs) (tmpdir </> mjs)
  bundle
    BundleTask
      { entry = tmpdir </> "rts.mjs",
        outputPath = tmpdir,
        outputFileName = "rts.js",
        outputLibrary = Just "Asterius"
      }
  BS.readFile $ tmpdir </> "rts.js"
