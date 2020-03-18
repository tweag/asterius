import Data.Array.IArray
import Data.Array.Unboxed

foreign import javascript "console.log($1)" js_print_int :: Int -> IO ()

facts :: [Int]
facts = scanl (*) 1 [1 ..]

boxedArr :: Array Int Int
boxedArr = listArray (0, 5) facts

unboxedArr :: UArray Int Int
unboxedArr = listArray (0, 5) facts

main :: IO ()
main = do
  js_print_int $ boxedArr ! 5
  js_print_int $ unboxedArr ! 5
