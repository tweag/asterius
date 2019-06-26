{-# OPTIONS_GHC -fforce-recomp #-}

module Main where
import Data.Char
import Control.Exception (assert, throwIO)
import GHC.Char (chr)
import Control.Monad(forM_)



assert_ :: String -> Bool -> IO ()
assert_ s True = return ()
assert_ s False = throwIO $ userError s

main :: IO ()
main = do
   -- Once we get top-level exceptions working, this will print a good
   -- correct error message!
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
