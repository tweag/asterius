#! stack runghc
import Data.List
import Data.Char

-- | Generate ASCII implementations of u_iswcntrl, u_iswprint by checking
-- | against GHC behaviour.


bool2int :: Bool -> Int
bool2int False = 0
bool2int True = 1

gencatArr :: [Int]
gencatArr = map (fromEnum . generalCategory . toEnum) [0..255]

iswcntrlArr :: [Int]
iswcntrlArr = map (bool2int . isControl . chr) [0..255]


iswprintArr :: [Int]
iswprintArr = map (bool2int . isControl . chr) [0..255]


intarrS :: [Int] -> String
intarrS as = "[" <> intercalate "," (map show as) <> "]"

lookupS :: String -- ^ Function name
  -> [Int] -- ^ Array to lookup
  -> String
lookupS fnname arr =
  fnname <> "(i) {" <>
  "\n\tvar lookup = " <> intarrS arr <> ";" <>
  "\n\treturn lookup[i];" <>
  "\n}"

main :: IO ()
main = do
   putStrLn $ lookupS "u_gencat" gencatArr
   putStrLn $ lookupS "u_iswcntrl" iswcntrlArr
   putStrLn $ lookupS "u_iswprint" iswprintArr
