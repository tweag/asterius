module Asterius.Iserv.Trace
  ( trace
  ) where

import Control.Monad
import System.Environment.Blank
import Text.Printf

trace :: Bool -> String -> IO ()
trace verbose s =
  when verbose $ getProgName >>= \name -> printf "[%20s] %s\n" name s
