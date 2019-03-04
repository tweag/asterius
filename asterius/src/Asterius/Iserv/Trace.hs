module Asterius.Iserv.Trace
  ( trace
  ) where

import Control.Monad
import System.Environment.Blank
import System.IO
import Text.Printf

trace :: Bool -> String -> IO ()
trace verbose s =
  when verbose $ getProgName >>= \name -> hPrintf stderr "[%20s] %s\n" name s
