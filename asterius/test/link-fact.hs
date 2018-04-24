{-# LANGUAGE OverloadedStrings #-}

import Asterius.LinkStart
import System.Directory
import System.FilePath
import Text.Show.Pretty

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  withCurrentDirectory (pwd </> "test" </> "fact-dump") $ do
    m <-
      linkStart
        defaultLinkStart {targets = ["Fact.hs"], rootSymbols = [undefined]}
    pPrint m
