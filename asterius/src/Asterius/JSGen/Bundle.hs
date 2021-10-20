{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.JSGen.Bundle
  ( BundleTask (..),
    bundle,
  )
where

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
