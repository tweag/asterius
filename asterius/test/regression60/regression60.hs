{-# OPTIONS_GHC -fforce-recomp #-}

module Main where

import Control.Exception
  ( assert,
    throwIO,
  )
import Control.Monad (forM_)
import Data.Char
import GHC.Char (chr)

assert_ :: String -> Bool -> IO ()
assert_ s True = return ()
assert_ s False = throwIO $ userError s

main :: IO ()
main = do
  assert_ "read 21" $ (read "21" :: Int) == 21
  assert_ "generalCategory of 2" $ generalCategory '2' == DecimalNumber
  assert_ "generalCategory of a" $ generalCategory 'a' == LowercaseLetter
  assert_ "generalCategory of A" $ generalCategory 'A' == UppercaseLetter
  assert_ "tolower of A" $ toLower 'A' == 'a'
  assert_ "tolower of a" $ toLower 'a' == 'a'
  assert_ "tolower of 1" $ toLower '1' == '1'
  assert_ "toupper of A" $ toUpper 'A' == 'A'
  assert_ "toupper of a" $ toUpper 'a' == 'A'
  assert_ "toupper of 1" $ toUpper '1' == '1'
-- Once we get top-level exceptions working, this will print a good
-- correct error message!
