{-# LANGUAGE RecordWildCards #-}

module GenUnicodeMJS
  ( main,
  )
where

import Control.Exception (assert)
import Data.Bits ((.|.), zeroBits)
import Data.Int (Int32)
import Data.List (findIndex, foldl', groupBy, intersperse, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Word (Word32, Word8)
import Numeric (readHex)

type Code = Word32

type CategoryFlag = Word32

data Category
  = Lu
  | Ll
  | Lt
  | Lm
  | Lo
  | Mn
  | Mc
  | Me
  | Nd
  | Nl
  | No
  | Pc
  | Pd
  | Ps
  | Pe
  | Pi
  | Pf
  | Po
  | Sm
  | Sc
  | Sk
  | So
  | Zs
  | Zl
  | Zp
  | Cc
  | Cf
  | Cs
  | Co
  | Cn
  deriving (Eq, Ord, Read, Show, Enum)

toFlag :: Category -> CategoryFlag
toFlag = (2 ^) . fromEnum

catUnion :: [Category] -> CategoryFlag
catUnion = foldl (.|.) zeroBits . map toFlag

f_cntrl, f_print, f_space, f_upper, f_lower, f_alpha, f_digit, f_alnum :: CategoryFlag
f_cntrl = toFlag Cc
f_print = catUnion [Lu .. Zs]
f_space = toFlag Zs
f_upper = toFlag Lu .|. toFlag Lt
f_lower = toFlag Ll
f_alpha = catUnion [Lu .. Lo]
f_digit = toFlag Nd
f_alnum = f_alpha .|. catUnion [Nd .. No]

flags :: [(String, CategoryFlag)]
flags = [("f_cntrl", f_cntrl), ("f_print", f_print), ("f_space", f_space), ("f_upper", f_upper), ("f_lower", f_lower), ("f_alpha", f_alpha), ("f_digit", f_digit), ("f_alnum", f_alnum)]

data Character
  = Character
      { c_code :: Code,
        c_generalCategory :: Category,
        c_simpleUppercaseMapping :: Code,
        c_simpleLowercaseMapping :: Code,
        c_simpleTitlecaseMapping :: Code
      }
  deriving (Eq, Show)

readCode :: String -> Code
readCode "" = 0
readCode c = case readHex c of
  [(i, "")] -> i
  _ -> error "parsing codepoint failed"

showCode :: Code -> String
showCode = show . toInteger

data Properties
  = Properties
      { p_generalCategory :: Category,
        p_toUpper :: Int32,
        p_toLower :: Int32,
        p_toTitle :: Int32
      }
  deriving (Show, Eq, Ord)

asProps :: Character -> Properties
asProps Character {..} = Properties
  { p_generalCategory = c_generalCategory,
    p_toUpper = distanceTo c_simpleUppercaseMapping,
    p_toLower = distanceTo c_simpleLowercaseMapping,
    p_toTitle = distanceTo c_simpleTitlecaseMapping
  }
  where
    distanceTo :: Code -> Int32
    distanceTo 0 = 0
    distanceTo c = fromInteger $ toInteger $ c - c_code

readCharacter :: String -> Character
readCharacter s = case splitOn ';' s of
  [ code,
    _name,
    generalCategory,
    _generalCombiningClass,
    _canonicalCombiningClass,
    _bidi_class,
    _decompositionType,
    _decompositionMapping,
    _numericType,
    _numericValue,
    _bidiMirrored,
    _isoComment,
    simpleUppercaseMapping,
    simpleLowercaseMapping,
    simpleTitlecaseMapping
    ] ->
      Character
        { c_code = readCode code,
          c_generalCategory = read generalCategory,
          c_simpleUppercaseMapping = readCode simpleUppercaseMapping,
          c_simpleLowercaseMapping = readCode simpleLowercaseMapping,
          c_simpleTitlecaseMapping = readCode simpleTitlecaseMapping
        }
  where
    splitOn :: Char -> String -> [String]
    splitOn sep = map (dropWhile (== sep)) . groupBy (\a b -> b /= sep)

toJS :: Character -> String
toJS Character {..} = "new Character(" ++ args ++ ")"
  where
    args =
      concat $
        intersperse
          ","
          [ showCode c_code,
            show c_generalCategory,
            showCode c_simpleUppercaseMapping,
            showCode c_simpleLowercaseMapping,
            showCode c_simpleTitlecaseMapping
          ]

data LookupBuilder
  = LookupBuilder
      { b_charBlocks :: [(Character, Word8)],
        b_propIndex :: Map.Map Properties Word8,
        b_nextIndex :: Word8
      }
  deriving (Eq, Show)

instance Semigroup LookupBuilder where
  l1 <> l2 = LookupBuilder
    { b_charBlocks = b_charBlocks l1 <> b_charBlocks l2,
      b_propIndex = b_propIndex l1 <> b_propIndex l2,
      b_nextIndex = max (b_nextIndex l1) (b_nextIndex l2)
    }

instance Monoid LookupBuilder where
  mempty = LookupBuilder [] Map.empty 0

include :: LookupBuilder -> Character -> LookupBuilder
include LookupBuilder {b_charBlocks = [], ..} char =
  LookupBuilder [(char, b_nextIndex)] (Map.singleton (asProps char) b_nextIndex) (succ b_nextIndex)
include l@LookupBuilder {b_charBlocks = ((blkStart, _) : _), ..} char@Character {..}
  | asProps blkStart == props = l
  | props `Map.member` b_propIndex = LookupBuilder [(char, b_propIndex Map.! props)] mempty 0 <> l
  | otherwise = LookupBuilder [(char, b_nextIndex)] (Map.singleton props b_nextIndex) (succ b_nextIndex) <> l
  where
    props = asProps char

data Lookup
  = Lookup
      { l_codeBlocks :: [Code],
        l_propIndex :: [Word8],
        l_generalCategory :: [Category],
        l_toUpper :: [Int32],
        l_toLower :: [Int32],
        l_toTitle :: [Int32]
      }
  deriving (Show)

build :: LookupBuilder -> Lookup
build LookupBuilder {..} = Lookup {..}
  where
    props = fst $ unzip $ sortOn snd $ Map.toList b_propIndex
    charBlocks = reverse b_charBlocks
    l_codeBlocks = map (c_code . fst) charBlocks
    l_propIndex = map snd charBlocks
    l_generalCategory = map p_generalCategory props
    l_toUpper = map p_toUpper props
    l_toLower = map p_toLower props
    l_toTitle = map p_toTitle props

toJS' :: Lookup -> [String]
toJS' Lookup {..} =
  [ "const _codeBlocks = Uint32Array.of(" ++ showArray l_codeBlocks ++ ");",
    "const _propIndex = Uint8Array.of(" ++ showArray l_propIndex ++ ");",
    "const _generalCategory = Uint32Array.of(" ++ showArray (map toFlag l_generalCategory) ++ ");",
    "const _toUpper = Int32Array.of(" ++ showArray l_toUpper ++ ");",
    "const _toLower = Int32Array.of(" ++ showArray l_toLower ++ ");",
    "const _toTitle = Int32Array.of(" ++ showArray l_toTitle ++ ");"
  ]
  where
    showArray :: (Show a) => [a] -> String
    showArray = concat . intersperse "," . map show

lookup' :: Lookup -> Code -> Properties
lookup' Lookup {..} code =
  let idx = case findIndex (> code) l_codeBlocks of
        Just idx -> idx - 1
        Nothing -> (length l_propIndex) - 1
      idx' = fromInteger (toInteger $ idx) :: Int
      idx'' = fromInteger $ toInteger $ l_propIndex !! idx'
   in Properties
        { p_generalCategory = l_generalCategory !! idx'',
          p_toLower = l_toLower !! idx'',
          p_toUpper = l_toUpper !! idx'',
          p_toTitle = l_toTitle !! idx''
        }

main :: IO ()
main = do
  db <- lines <$> getContents
  let chars = map readCharacter db
  mapM_ (\c -> putStrLn $ (show $ c_code c) ++ " " ++ (show $ toFlag $ c_generalCategory c)) chars
  let look = build $ foldl' include mempty chars
  let mismatches = [(c_code char, asProps char, lookup' look (c_code char)) | char <- chars, asProps char /= lookup' look (c_code char)]
  mapM_ print mismatches
-- mapM_ putStrLn (toJS' look)
-- print (length characters)
-- let c = map readCharacter l
-- let pmap = Map.fromList [(asProps ch, 1) | ch <- c]
-- print $ Map.size pmap
-- let catRanges = map (\l -> (c_code $ head l, c_code $ last l)) $ groupBy (\a b -> asProps a == asProps b) characters
-- let lengths = map (\(a, b) -> b - a) catRanges
-- let e = length $ filter (== 0) lengths
-- print e
-- mapM_ (print . \(a, b) -> b - a) catRanges
-- let m = Map.fromDistinctAscList [(c_code ch, ch) | ch <- c]
-- let notUpperLower = [ch | ch <- c, c_simpleUppercaseMapping ch /= 0, c_simpleLowercaseMapping (m Map.! (c_simpleUppercaseMapping ch)) /= c_code ch]
-- let notTitleLower = [ch | ch <- c, c_simpleTitlecaseMapping ch /= 0, c_simpleLowercaseMapping (m Map.! (c_simpleTitlecaseMapping ch)) /= c_code ch]
-- putStrLn "notUpperLower"
-- mapM_ (putStrLn . show) notUpperLower
-- putStrLn "notTitleLower"
-- mapM_ (putStrLn . show) notTitleLower
-- mapM_ (putStrLn . (++ ",") . toJS) c
-- mapM_ (putStrLn . uncurry (\n f -> "const _" ++ n ++ " = " ++ show f ++ ";")) flags

-- Line format (columns separated by ';'):
-- 0. Code
-- 1. Name
-- 2. General_Category
-- 3. Canonical_Combining_Class
-- 4. Bidi_Class
-- 5. Decomposition_Type
-- 6. Decomposition_Mapping
-- 7. Numeric_Type
-- 8. Numeric_Value
-- 9. Bidi_Mirrored
-- 10. Unicode_1_Name
-- 11. ISO_Comment
-- 12. Simple_Uppercase_Mapping
-- 13. Simple_Lowercase_Mapping
-- 14. Simple_Titlecase_Mapping

-- 01C5;
-- LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON;
-- Lt;
-- 0;
-- L;
-- <compat> 0044 017E;
-- ;
-- ;
-- ;
-- N;
-- LATIN LETTER CAPITAL D SMALL Z HACEK;
-- ;
-- 01C4;
-- 01C6;
-- 01C5
