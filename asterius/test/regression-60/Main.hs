module Main where
import Data.Char
import GHC.Unicode

main :: IO ()
main = do
   -- We need to implement unicode primitives.
   -- See implementation in eta:
   -- https://github.com/typelead/eta/blob/8c1e23308b4b3951e651889f3639a38204c6d33d/libraries/base/java-utils/Utils.java#L97
   putStrLn "testing wgencat..."
   let x = wgencat $ ord '1'
   print x

   -- putStrLn "testing read..."
   -- let y = read "12" :: Int
   -- print y

