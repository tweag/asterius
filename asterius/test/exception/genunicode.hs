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

-- | This script generates a JavaScript module that can classify and convert
-- Unicode code points. The generated module contains one class, called
-- Unicode, with the following methods:
--
-- > class Unicode {
-- >   u_gencat(code)
-- >   u_iswupper(code)
-- >   u_iswlower(code)
-- >   u_iswspace(code)
-- >   u_iswalpha(code)
-- >   u_iswdigit(code)
-- >   u_iswalnum(code)
-- >   u_iswprint(code)
-- >   u_iswcntrl(code)
-- >   u_towlower(code)
-- >   u_towupper(code)
-- >   u_towtitle(code)
-- > }
--
-- In order to implement these methods, we need to know, for each Unicode code
-- point:
--
-- * its "general category", for example lowercase letter, digit, or control
--   character;
-- * how to find the code point representing its uppercase, lowercase, and
--   titlecase version (if applicable).
--
-- The Unicode standard defines just under 33,000 code points, so lookup tables
-- mapping each code point directly to its properties need a lot of space. This
-- script exploits patterns in the code point data to represent it using less
-- space.
--
-- Firstly, there are many characters whose code point conversions are equal in
-- terms of code point _difference_. As an example, to go from any capital
-- ASCII to its lowercase code point, just add 32.
--
-- 'Properties' thus represents the uppercase, lowercase and titlecase
-- conversion as (relative) code point differences, rather than (absolute) code
-- points. In addition, 'Properties' contains the general category of the
-- character.
--
-- Represented in such a way, the characteristics of any Unicode character can
-- be represented as one of just over 200 different 'Properties' instances.
--
-- Secondly, characters with adjacent code points often share the same
-- properties. We exploit this by associating 'Properties' not with characters,
-- but with 'Block's of characters.
--
-- In order to avoid object creation and hashing in the JavaScript
-- implementation, we use a "structure of arrays" representation using only
-- "typed" (that is, integer) arrays:
--
-- * Each 'Block' is represented as an entry @i@ in the following arrays:
--     * @first[i]@, containing the first character in the block,
--     * @last[i]@, containing the last character in the block, and
--     * @idx[i]@, containing the index into the property tables.
--
-- * 'Properties' are represented as an entry @j@ in the following arrays:
--     * @gencat[j]@, containing the numeric code for the general category,
--     * @tolower[j]@, @toupper[j]@, and @totitle[j]@, containing the code
--       point difference to the relevant lowercase, uppercase, and titlecase
--       character, respectively.
--
-- This compact representation complicates accessing properties, but not too
-- much so. Here is how the JavaScript implementation finds the properties of
-- a Unicode character:
--
-- 1. Find @i@ such that @first[i] <= code <= last[i]@ via binary search;
-- 2. @gencat[idx[i]]@ is the general category of the character.

-- | Relevant properties of a Unicode character.
data Properties
  = Properties
      { -- | Unicode "general property" of the character
        p_generalCategory :: Char.GeneralCategory,
        -- | Add this to the code point to get to its uppercase code point
        p_toUpper :: Int32,
        -- | Add this to the code point to get to its lowercase code point
        p_toLower :: Int32,
        -- | Add this to the code point to get to its titlecase code point
        p_toTitle :: Int32
      }
  deriving (Eq, Ord)

-- | Block of adjacent Unicode characters with the same Unicode properties.
data Block
  = Block
      { -- | First character in the block (inclusive)
        b_first :: Char,
        -- | Last character in the block (inclusive)
        b_last :: Char,
        -- | Index into the properties arrays
        b_index :: Word8
      }
  deriving (Eq)

-- | Used to build an instance of 'Lookup'.
data LookupBuilder
  = LookupBuilder
      { -- | Non-overlapping blocks of characters, in descending order
        b_blocks :: [Block],
        -- | Maps each 'Properties' to its index in the final array in 'Lookup'
        b_propIndex :: Map.Map Properties Word8,
        -- | Index to associate the next unseen 'Properties' with
        b_nextIndex :: Word8
      }

instance Semigroup LookupBuilder where
  l1 <> l2 = LookupBuilder
    { b_blocks = b_blocks l1 <> b_blocks l2,
      b_propIndex = b_propIndex l1 <> b_propIndex l2,
      b_nextIndex = max (b_nextIndex l1) (b_nextIndex l2)
    }

instance Monoid LookupBuilder where
  mempty = LookupBuilder [] Map.empty 0

-- | Lookup tables for Unicode character properties. 'l_first', 'l_last' and
-- 'l_propIndex' have the same length and together represent adjacent blocks
-- of characters which share all properties.
--
-- 'l_generalCategory', 'l_toUpper', 'l_toLower' and 'l_toTitle' together
-- represent character properties and have the same length.
--
-- 'l_propIndex' is the index into the property lists. As an example, to
-- find the general category of "X", find i such that l_first[i] <= "X" and
-- X <= l_last[i]. Then, l_generalCategory[l_propIndex[i]] will be the
-- general category of "X".
data Lookup
  = Lookup
      { -- | First characters of blocks (inclusive)
        l_first :: [Char],
        -- | Last characters of blocks (inclusive)
        l_last :: [Char],
        -- | Index into properties arrays
        l_propIndex :: [Word8],
        -- | Unicode "general category"
        l_generalCategory :: [Char.GeneralCategory],
        -- | Add this to the code point to get to its uppercase code point
        l_toUpper :: [Int32],
        -- | Add this to the code point to get to its lowercase code point
        l_toLower :: [Int32],
        -- | Add this to the code point to get to its titlecase code point
        l_toTitle :: [Int32]
      }

-- | Create the 'Lookup' table for all Unicode characters.
mkLookup ::
  -- | Not every 'Char' represents a Unicode character. This function should map
  -- each valid Char to its 'Properties'.
  (Char -> Maybe Properties) ->
  Lookup
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

-------------------------------------------------------------------------------
-- Unicode properties

-- | Define the "general categories" we care about.
isRelevantCategory :: Char.GeneralCategory -> Bool
isRelevantCategory Char.Surrogate = False
isRelevantCategory Char.PrivateUse = False
isRelevantCategory Char.NotAssigned = False
isRelevantCategory _ = True

-- | Determine 'Properties' using the base library.
ghcProps :: Char -> Maybe Properties
ghcProps c | not (isRelevantCategory (Char.generalCategory c)) = Nothing
ghcProps c = Just Properties
  { p_generalCategory = Char.generalCategory c,
    p_toUpper = fromInteger $ toInteger $ fromEnum (Char.toUpper c) - fromEnum c,
    p_toLower = fromInteger $ toInteger $ fromEnum (Char.toLower c) - fromEnum c,
    p_toTitle = fromInteger $ toInteger $ fromEnum (Char.toTitle c) - fromEnum c
  }

-------------------------------------------------------------------------------
-- Generating the JavaScript module

-- | Combinations of 'Char.GeneralCategory'.
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

-- | Flag representing the union of the given 'Char.GeneralCategory's.
categoryUnion :: [Char.GeneralCategory] -> Word32
categoryUnion = foldl (.|.) zeroBits . map categoryFlag
  where
    categoryFlag :: Char.GeneralCategory -> Word32
    categoryFlag = (2 ^) . fromEnum

-- | Flag representing the given 'Kind'.
kindFlag :: Kind -> Word32
kindFlag Upper = categoryUnion [Char.UppercaseLetter, Char.TitlecaseLetter]
kindFlag Lower = categoryUnion [Char.LowercaseLetter]
kindFlag Space = categoryUnion [Char.Space]
kindFlag Alpha = categoryUnion [Char.UppercaseLetter .. Char.OtherLetter]
kindFlag Digit = categoryUnion [Char.DecimalNumber]
kindFlag Alnum = kindFlag Alpha .|. categoryUnion [Char.DecimalNumber .. Char.OtherNumber]
kindFlag Print = categoryUnion [Char.UppercaseLetter .. Char.Space]
kindFlag Cntrl = categoryUnion [Char.Control]

-- | Represents the different types of JavaScript Typed Arrays.
data JsArrayType = Uint8 | Uint16 | Uint32 | Int8 | Int16 | Int32
  deriving (Show)

-- | JavaScript template.
js :: Lookup -> [String]
js Lookup {..} =
  concat
    [ ["// prettier-ignore", line]
      | line <-
          [ "const _first = " ++ mkTypedJsArray Uint32 l_first ++ ";",
            "const _last = " ++ mkTypedJsArray Uint32 l_last ++ ";",
            "const _idx = " ++ mkTypedJsArray Uint8 l_propIndex ++ ";",
            "const _gencat = " ++ mkTypedJsArray Uint8 l_generalCategory ++ ";",
            "const _toupper = " ++ mkTypedJsArray Int32 l_toUpper ++ ";",
            "const _tolower = " ++ mkTypedJsArray Int32 l_toLower ++ ";",
            "const _totitle = " ++ mkTypedJsArray Int32 l_toTitle ++ ";"
          ]
    ]
    ++ [ "",
         "function _bbsearch(key, start, end) {",
         "  const isBaseCase = start + 1 == end;",
         "  const pivot = ~~((start + end) / 2)",
         "  if (key < _first[pivot]) {",
         "    return isBaseCase ? -1 : _bbsearch(key, start, pivot);",
         "  } else if (key <= _last[pivot]) {",
         "    return pivot;",
         "  } else {",
         "    return isBaseCase ? -1 : _bbsearch(key, pivot, end);",
         "  }",
         "}",
         "",
         "function _property(table, c) {",
         "  const idx = _bbsearch(c, 0, c + 1);",
         "  return idx == -1 ? 0 : table[_idx[idx]];",
         "}",
         "",
         "export class Unicode {",
         "  constructor(logger) {",
         "    Object.seal(this);",
         "  }",
         "",
         "  u_gencat(c) {",
         "    return _property(_gencat, c);",
         "  }"
       ]
    ++ concat
      [ [ "",
          "  u_isw" ++ map Char.toLower (show kind) ++ "(c) {",
          "    return !!((1 << this.u_gencat(c)) & " ++ show (kindFlag kind) ++ ");",
          "  }"
        ]
        | kind <- kinds
      ]
    ++ concat
      [ [ "",
          "  u_tow" ++ capitalisation ++ "(c) {",
          "    return c + _property(_to" ++ capitalisation ++ ", c);",
          "  }"
        ]
        | capitalisation <- ["lower", "upper", "title"]
      ]
    ++ ["}"]
  where
    kinds :: [Kind]
    kinds = [minBound ..]
    mkTypedJsArray :: Enum a => JsArrayType -> [a] -> String
    mkTypedJsArray typ xs = show typ ++ "Array.of(" ++ concat (List.intersperse "," (map (show . fromEnum) xs)) ++ ")"

-------------------------------------------------------------------------------
-- Do it!

main :: IO ()
main = do
  progName <- getProgName
  putStrLn $ "// Generated by " ++ progName
  putStrLn ""
  mapM_ putStrLn $ js $ mkLookup $ ghcProps
