{-# LANGUAGE RecordWildCards #-}

import Data.Bits ((.|.), zeroBits)
import qualified Data.Char as Char
import Data.Int (Int32)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)
import Data.Word (Word32, Word8)
import Numeric (readHex)
import System.Environment (getProgName)

-- Generating the lookup table
data Properties
  = Properties
      { p_generalCategory :: Char.GeneralCategory,
        p_toUpper :: Int32,
        p_toLower :: Int32,
        p_toTitle :: Int32
      }
  deriving (Eq, Ord)

data Block
  = Block
      { b_first :: Char,
        b_last :: Char,
        b_index :: Word8
      }
  deriving (Eq)

data LookupBuilder
  = LookupBuilder
      { b_blocks :: [Block],
        b_propIndex :: Map.Map Properties Word8,
        b_nextIndex :: Word8
      }

data Lookup
  = Lookup
      { l_first :: [Char],
        l_last :: [Char],
        l_propIndex :: [Word8],
        l_generalCategory :: [Char.GeneralCategory],
        l_toUpper :: [Int32],
        l_toLower :: [Int32],
        l_toTitle :: [Int32]
      }

instance Semigroup LookupBuilder where
  l1 <> l2 = LookupBuilder
    { b_blocks = b_blocks l1 <> b_blocks l2,
      b_propIndex = b_propIndex l1 <> b_propIndex l2,
      b_nextIndex = max (b_nextIndex l1) (b_nextIndex l2)
    }

instance Monoid LookupBuilder where
  mempty = LookupBuilder [] Map.empty 0

mkLookup :: (Char -> Maybe Properties) -> Lookup
mkLookup props = build $ List.foldl' addChar mempty [minBound ..]
  where
    props' :: Char -> Properties
    props' = fromJust . props
    addChar :: LookupBuilder -> Char -> LookupBuilder
    addChar builder char | isNothing (props char) = builder
    addChar LookupBuilder {b_blocks = [], ..} char =
      LookupBuilder [Block char char b_nextIndex] (Map.singleton (props' char) b_nextIndex) (succ b_nextIndex)
    addChar l@LookupBuilder {b_blocks = Block {..} : bs, ..} char
      | p == props' b_first =
        l {b_blocks = Block {b_last = char, ..} : bs}
      | p `Map.member` b_propIndex =
        LookupBuilder [Block char char (b_propIndex Map.! p)] mempty 0 <> l
      | otherwise =
        LookupBuilder [Block char char b_nextIndex] (Map.singleton p b_nextIndex) (succ b_nextIndex) <> l
      where
        p = props' char
    build :: LookupBuilder -> Lookup
    build LookupBuilder {..} = Lookup {..}
      where
        props = fst $ unzip $ List.sortOn snd $ Map.toList b_propIndex
        blocks = reverse b_blocks
        l_first = map b_first blocks
        l_last = map b_last blocks
        l_propIndex = map b_index blocks
        l_generalCategory = map p_generalCategory props
        l_toUpper = map p_toUpper props
        l_toLower = map p_toLower props
        l_toTitle = map p_toTitle props

-- Figuring out props
ghcProps :: Char -> Maybe Properties
ghcProps c = Just Properties
  { p_generalCategory = Char.generalCategory c,
    p_toUpper = fromInteger $ toInteger $ fromEnum (Char.toUpper c) - fromEnum c,
    p_toLower = fromInteger $ toInteger $ fromEnum (Char.toLower c) - fromEnum c,
    p_toTitle = fromInteger $ toInteger $ fromEnum (Char.toTitle c) - fromEnum c
  }

unicodeDataProps :: [String] -> Char -> Maybe Properties
unicodeDataProps db = flip Map.lookup (Map.fromAscList (map parse db))
  where
    readCode :: String -> Maybe Char
    readCode "" = Nothing
    readCode c = case readHex c of
      [(i, "")] -> Just (toEnum i)
      _ -> error "parsing codepoint failed"
    readGeneralCategory :: String -> Char.GeneralCategory
    readGeneralCategory s = toEnum $ fromJust $ List.findIndex (== s) ["Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc", "Me", "Nd", "Nl", "No", "Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po", "Sm", "Sc", "Sk", "So", "Zs", "Zl", "Zp", "Cc", "Cf", "Cs", "Co", "Cn"]
    splitOn :: Char -> String -> [String]
    splitOn sep = map (dropWhile (== sep)) . List.groupBy (\a b -> b /= sep)
    distance :: Char -> Maybe Char -> Int32
    distance from Nothing = 0
    distance from (Just to) = toEnum $ fromEnum to - fromEnum from
    parse :: String -> (Char, Properties)
    parse s = case splitOn ';' s of
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
          ( c,
            Properties
              { p_generalCategory = readGeneralCategory generalCategory,
                p_toUpper = distance c (readCode simpleUppercaseMapping),
                p_toLower = distance c (readCode simpleLowercaseMapping),
                p_toTitle = distance c (readCode simpleTitlecaseMapping)
              }
          )
          where
            Just c = readCode code

-- Generating the JS
data Kind
  = Upper
  | Lower
  | Space
  | Alpha
  | Digit
  | Alnum
  | Print
  | Cntrl
  deriving (Show, Enum, Bounded)

categoryUnion :: [Char.GeneralCategory] -> Word32
categoryUnion = foldl (.|.) zeroBits . map categoryFlag
  where
    categoryFlag :: Char.GeneralCategory -> Word32
    categoryFlag = (2 ^) . fromEnum

kindFlag :: Kind -> Word32
kindFlag Upper = categoryUnion [Char.UppercaseLetter, Char.TitlecaseLetter]
kindFlag Lower = categoryUnion [Char.LowercaseLetter]
kindFlag Space = categoryUnion [Char.Space]
kindFlag Alpha = categoryUnion [Char.UppercaseLetter .. Char.OtherLetter]
kindFlag Digit = categoryUnion [Char.DecimalNumber]
kindFlag Alnum = kindFlag Alpha .|. categoryUnion [Char.DecimalNumber .. Char.OtherNumber]
kindFlag Print = categoryUnion [Char.UppercaseLetter .. Char.Space]
kindFlag Cntrl = categoryUnion [Char.Control]

data JsArrayType = Uint8 | Uint16 | Uint32 | Int8 | Int16 | Int32
  deriving (Show)

js :: Lookup -> [String]
js Lookup {..} =
  [ "const _first = " ++ mkTypedJsArray Uint32 l_first ++ ";",
    "const _last = " ++ mkTypedJsArray Uint32 l_last ++ ";",
    "const _idx = " ++ mkTypedJsArray Uint8 l_propIndex ++ ";",
    "const _gencat = " ++ mkTypedJsArray Uint8 l_generalCategory ++ ";",
    "const _toupper = " ++ mkTypedJsArray Int32 l_toUpper ++ ";",
    "const _tolower = " ++ mkTypedJsArray Int32 l_toLower ++ ";",
    "const _totitle = " ++ mkTypedJsArray Int32 l_toTitle ++ ";",
    "",
    "function _bbsearch(key, start, end) {",
    "    const isBaseCase = start + 1 == end;",
    "    const pivot = ~~((start + end) / 2)",
    "    if (key < _first[pivot]) {",
    "        return isBaseCase ? -1 : _bbsearch(key, start, pivot);",
    "    } else if (key <= _last[pivot]) {",
    "        return pivot;",
    "    } else {",
    "        return isBaseCase ? -1 : _bbsearch(key, pivot, end);",
    "    }",
    "}",
    "",
    "function _property(table, c) {",
    "    const idx = _bbsearch(c, 0, c + 1);",
    "    return idx == -1 ? 0 : table[_idx[idx]];",
    "}",
    "",
    "export class Unicode {",
    "    constructor(logger) {",
    "        Object.seal(this);",
    "    }",
    "",
    "    u_gencat(c) {",
    "        return _property(_gencat, c);",
    "    }"
  ]
    ++ concat
      [ [ "",
          "    u_isw" ++ map Char.toLower (show kind) ++ "(c) {",
          "        return !!((1 << this.u_gencat(c)) & " ++ show (kindFlag kind) ++ ");",
          "    }"
        ]
        | kind <- kinds
      ]
    ++ concat
      [ [ "",
          "    u_tow" ++ capitalisation ++ "(c) {",
          "        return c + _property(_to" ++ capitalisation ++ ", c);",
          "    }"
        ]
        | capitalisation <- ["lower", "upper", "title"]
      ]
    ++ ["}"]
  where
    kinds :: [Kind]
    kinds = [minBound ..]
    mkTypedJsArray :: Enum a => JsArrayType -> [a] -> String
    mkTypedJsArray typ xs = show typ ++ "Array.of(" ++ concat (List.intersperse "," (map (show . fromEnum) xs)) ++ ")"

main :: IO ()
main = do
  progName <- getProgName
  putStrLn $ "// Generated using " ++ progName
  db <- getContents
  mapM_ putStrLn $ js $ mkLookup $ unicodeDataProps $ lines db
-- mapM_ putStrLn $ js $ mkLookup ghcProps
