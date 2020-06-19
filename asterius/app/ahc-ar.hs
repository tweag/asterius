-- |
-- Module      :  Main
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- This module contains the implementation of @ahc-ar@, a system-agnostic,
-- partial implementation of GNU @ar@ for creating archive files from a set of
-- object files, using @Ar@ from the GHC API.
module Main
  ( main,
  )
where

import Asterius.Ar (createArchive)
import Data.List
import System.Environment.Blank
import System.Exit
import System.IO.Error

main :: IO ()
main = do
  args <- getAhcArArgs
  case find (".a" `isSuffixOf`) args of
    Just ar -> createArchive ar $ filter (".o" `isSuffixOf`) args
    Nothing -> die "ahc-ar: no .a file passed. Exiting..."

-- | Get all command-line arguments. Arguments of the form @\@file@ are
-- replaced by the newline-separated options contained in @file@. If reading
-- @file@ fails for any reason, the original @\@file@ argument remains intact.
getAhcArArgs :: IO [String]
getAhcArArgs = getArgs >>= fmap concat . mapM expand
  where
    expand :: String -> IO [String]
    expand opt = case opt of
      ('@' : path) ->
        catchIOError
          (map undoEscapeResponseFileArg . lines <$> readFile path)
          (const $ pure [opt])
      _ -> return [opt]

-- | Un-escape a single the contents of @.rsp@ file, essentially reversing the
-- effects of Cabal's @escapeResponseFileArg@
-- (https://github.com/haskell/cabal/blob/dde6255a4e6b531c0567bc499840d11ead9d885b/Cabal/Distribution/Simple/Program/ResponseFile.hs#L48).
undoEscapeResponseFileArg :: String -> String
undoEscapeResponseFileArg arg = case arg of
  "" -> ""
  '\\' : c : cs -> c : undoEscapeResponseFileArg cs
  '\\' : [] -> error "undoEscapeResponseFileArg: dangling backslash"
  c : cs -> c : undoEscapeResponseFileArg cs
