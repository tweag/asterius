{-# LANGUAGE RecordWildCards #-}

module GenUnicodeMJS
  ( main,
  )
where

import Control.Exception (assert)
import Data.Bits ((.|.), zeroBits)
import qualified Data.Char as Char
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

data CompoundCategory
  = Upper
  | Lower
  | Space
  | Alpha
  | Digit
  | Alnum
  | Print
  | Cntrl
  deriving (Show, Bounded, Enum)

flag :: CompoundCategory -> CategoryFlag
flag Upper = toFlag Lu .|. toFlag Lt
flag Lower = toFlag Ll
flag Space = toFlag Zs
flag Alpha = catUnion [Lu .. Lo]
flag Digit = toFlag Nd
flag Alnum = flag Alpha .|. catUnion [Nd .. No]
flag Print = catUnion [Lu .. Zs]
flag Cntrl = toFlag Cc

jsFlag :: CompoundCategory -> String
jsFlag cc = "const _f_" ++ map Char.toLower (show cc) ++ " = " ++ show (flag cc) ++ ";"

jsFlagFunction :: CompoundCategory -> String
jsFlagFunction cc =
  unlines
    [ "function u_isw" ++ cat ++ "(code) {",
      "  return _property(_generalCategory, code) & _f_" ++ cat ++ ";",
      "}"
    ]
  where
    cat = map Char.toLower (show cc)

jsConversionFunction :: String -> String
jsConversionFunction cat =
  unlines
    [ "function u_tow" ++ cat ++ "(code) {",
      "  return code + _property(_to" ++ cat ++ ", code);",
      "}"
    ]

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

data Block
  = Block
      { b_first :: Character,
        b_last :: Character,
        b_index :: Word8
      }
  deriving (Eq, Show)

data LookupBuilder
  = LookupBuilder
      { b_blocks :: [Block],
        b_propIndex :: Map.Map Properties Word8,
        b_nextIndex :: Word8
      }
  deriving (Eq, Show)

instance Semigroup LookupBuilder where
  l1 <> l2 = LookupBuilder
    { b_blocks = b_blocks l1 <> b_blocks l2,
      b_propIndex = b_propIndex l1 <> b_propIndex l2,
      b_nextIndex = max (b_nextIndex l1) (b_nextIndex l2)
    }

instance Monoid LookupBuilder where
  mempty = LookupBuilder [] Map.empty 0

-- Assumes ascending order of Characters!
include :: LookupBuilder -> Character -> LookupBuilder
include LookupBuilder {b_blocks = [], ..} char =
  LookupBuilder [Block char char b_nextIndex] (Map.singleton (asProps char) b_nextIndex) (succ b_nextIndex)
include l@LookupBuilder {b_blocks = Block {..} : bs, ..} char@Character {..}
  | asProps b_first == props = l {b_blocks = Block {b_last = char, ..} : bs}
  | props `Map.member` b_propIndex = LookupBuilder [Block char char (b_propIndex Map.! props)] mempty 0 <> l
  | otherwise = LookupBuilder [Block char char b_nextIndex] (Map.singleton props b_nextIndex) (succ b_nextIndex) <> l
  where
    props = asProps char

mkLookup :: [Character] -> Lookup
mkLookup = build . foldl' include mempty

ghcProps :: Char -> Properties
ghcProps c = Properties
  { p_generalCategory = toEnum $ fromEnum $ Char.generalCategory c,
    p_toUpper = fromInteger $ toInteger $ fromEnum (Char.toUpper c) - fromEnum c,
    p_toLower = fromInteger $ toInteger $ fromEnum (Char.toLower c) - fromEnum c,
    p_toTitle = fromInteger $ toInteger $ fromEnum (Char.toTitle c) - fromEnum c
  }

data Lookup
  = Lookup
      { l_blockFirst :: [Code],
        l_blockLast :: [Code],
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
    blocks = reverse b_blocks
    l_blockFirst = map (c_code . b_first) blocks
    l_blockLast = map (c_code . b_last) blocks
    l_propIndex = map b_index blocks
    l_generalCategory = map p_generalCategory props
    l_toUpper = map p_toUpper props
    l_toLower = map p_toLower props
    l_toTitle = map p_toTitle props

jsPrelude :: String
jsPrelude =
  unlines
    [ "function _bbsearch(key, first, last, start, end) {",
      "  const isBaseCase = start + 1 == end;",
      "  const pivot = ~~((start + end) / 2)",
      "  if (key < first[pivot]) {",
      "    return isBaseCase ? -1 : _bbsearch(key, first, last, start, pivot);",
      "  } else if (key <= last[pivot]) {",
      "    return pivot;",
      "  } else {",
      "    return isBaseCase ? -1 : _bbsearch(key, first, last, pivot, end);",
      "  }",
      "}",
      "",
      "function _property(propTable, code) {",
      "  const idx = _bbsearch(code, _blockFirst, _blockLast, 0, code + 1);",
      "  return idx == -1 ? 0 : propTable[_propIndex[idx]];",
      "}"
    ]

toJS' :: Lookup -> [String]
toJS' Lookup {..} =
  [ "const _blockFirst = Uint32Array.of(" ++ showArray l_blockFirst ++ ");",
    "const _blockLast = Uint32Array.of(" ++ showArray l_blockLast ++ ");",
    "const _propIndex = Uint8Array.of(" ++ showArray l_propIndex ++ ");",
    "const _generalCategory = Uint32Array.of(" ++ showArray (map toFlag l_generalCategory) ++ ");",
    "const _toupper = Int32Array.of(" ++ showArray l_toUpper ++ ");",
    "const _tolower = Int32Array.of(" ++ showArray l_toLower ++ ");",
    "const _totitle = Int32Array.of(" ++ showArray l_toTitle ++ ");"
  ]
  where
    showArray :: Show a => [a] -> String
    showArray = concat . intersperse "," . map show

lookup' :: Lookup -> Code -> Properties
lookup' Lookup {..} code =
  let idx = case findIndex (> code) l_blockFirst of
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
  mapM_ putStrLn $ toJS' $ mkLookup chars
  putStrLn ""
  putStrLn jsPrelude
  mapM_ (putStrLn . jsFlag) [minBound ..]
  putStrLn ""
  mapM_ (putStrLn . jsFlagFunction) [minBound ..]
  mapM_ (putStrLn . jsConversionFunction) ["lower", "upper", "title"]
