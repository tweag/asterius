module Data.Time.Format.Parse.Class
    (
        -- * Parsing
        ParseNumericPadding(..),
        ParseTime(..),
        parseSpecifiers,
        timeSubstituteTimeSpecifier,
        timeParseTimeSpecifier,
        durationParseTimeSpecifier,
    )
    where

import Control.Applicative hiding (optional,many)
import Data.Char
import Data.Maybe
import Data.Time.Format.Locale
import Text.ParserCombinators.ReadP

data ParseNumericPadding = NoPadding | SpacePadding | ZeroPadding

-- | The class of types which can be parsed given a UNIX-style time format
-- string.
class ParseTime t where
    -- | @since 1.9.1
    substituteTimeSpecifier :: proxy t -> TimeLocale -> Char -> Maybe String
    substituteTimeSpecifier _ _ _ = Nothing
    -- | Get the string corresponding to the given format specifier.
    --
    -- @since 1.9.1
    parseTimeSpecifier :: proxy t -> TimeLocale -> Maybe ParseNumericPadding -> Char -> ReadP String
    -- | Builds a time value from a parsed input string.
    -- If the input does not include all the information needed to
    -- construct a complete value, any missing parts should be taken
    -- from 1970-01-01 00:00:00 +0000 (which was a Thursday).
    -- In the absence of @%C@ or @%Y@, century is 1969 - 2068.
    --
    -- @since 1.9.1
    buildTime :: TimeLocale -- ^ The time locale.
              -> [(Char,String)] -- ^ Pairs of format characters and the
                                 -- corresponding part of the input.
              -> Maybe t

-- | Case-insensitive version of 'Text.ParserCombinators.ReadP.char'.
charCI :: Char -> ReadP Char
charCI c = satisfy (\x -> toUpper c == toUpper x)

-- | Case-insensitive version of 'Text.ParserCombinators.ReadP.string'.
stringCI :: String -> ReadP String
stringCI this = do
    let
        scan [] _ = return this
        scan (x:xs) (y:ys) | toUpper x == toUpper y = do
            _ <- get
            scan xs ys
        scan _ _ = pfail
    s <- look
    scan this s

parseSpecifiers :: ParseTime t => proxy t -> TimeLocale -> String -> ReadP [(Char,String)]
parseSpecifiers pt locale = let
    parse :: String -> ReadP [(Char,String)]
    parse [] = return []
    parse ('%':cs) = parse1 cs
    parse (c:cs) | isSpace c = do
        _ <- satisfy isSpace
        case cs of
            (c':_) | isSpace c' -> return ()
            _ -> skipSpaces
        parse cs
    parse (c:cs) = do
        _ <- charCI c
        parse cs

    parse1 :: String -> ReadP [(Char,String)]
    parse1 ('-':cs) = parse2 (Just NoPadding) cs
    parse1 ('_':cs) = parse2 (Just SpacePadding) cs
    parse1 ('0':cs) = parse2 (Just ZeroPadding) cs
    parse1 cs = parse2 Nothing cs

    parse2 :: Maybe ParseNumericPadding -> String -> ReadP [(Char,String)]
    parse2 mpad ('E':cs) = parse3 mpad True cs
    parse2 mpad cs = parse3 mpad False cs

    parse3 :: Maybe ParseNumericPadding -> Bool -> String -> ReadP [(Char,String)]
    parse3 _ _ ('%':cs) = do
        _ <- char '%'
        parse cs
    parse3 _ _ (c:cs) | Just s <- substituteTimeSpecifier pt locale c = parse $ s ++ cs
    parse3 mpad _alt (c:cs) = do
        str <- parseTimeSpecifier pt locale mpad c
        specs <- parse cs
        return $ (c,str) : specs
    parse3 _ _ [] = return []
    in parse

parsePaddedDigits :: ParseNumericPadding -> Int -> ReadP String
parsePaddedDigits ZeroPadding n = count n (satisfy isDigit)
parsePaddedDigits SpacePadding _n = skipSpaces >> many1 (satisfy isDigit)
parsePaddedDigits NoPadding _n = many1 (satisfy isDigit)

parsePaddedSignedDigits :: ParseNumericPadding -> Int -> ReadP String
parsePaddedSignedDigits pad n = do
    sign <- option "" $ char '-' >> return "-"
    digits <- parsePaddedDigits pad n
    return $ sign ++ digits

parseSignedDecimal :: ReadP String
parseSignedDecimal = do
    sign <- option "" $ char '-' >> return "-"
    skipSpaces
    digits <- many1 $ satisfy isDigit
    decimaldigits <- option "" $ do
        _ <- char '.'
        dd <- many $ satisfy isDigit
        return $ '.':dd
    return $ sign ++ digits ++ decimaldigits

timeParseTimeSpecifier :: TimeLocale -> Maybe ParseNumericPadding -> Char -> ReadP String
timeParseTimeSpecifier l mpad c = let
    digits pad = parsePaddedDigits (fromMaybe pad mpad)
    oneOf = choice . map stringCI
    numericTZ = do
        s <- choice [char '+', char '-']
        h <- parsePaddedDigits ZeroPadding 2
        optional (char ':')
        m <- parsePaddedDigits ZeroPadding 2
        return (s:h++m)
    in case c of
        -- century
        'C' -> digits SpacePadding 2
        'f' -> digits SpacePadding 2

        -- year
        'Y' -> digits SpacePadding 4
        'G' -> digits SpacePadding 4

        -- year of century
        'y' -> digits ZeroPadding 2
        'g' -> digits ZeroPadding 2

        -- month of year
        'B' -> oneOf (map fst (months l))
        'b' -> oneOf (map snd (months l))
        'm' -> digits ZeroPadding 2

        -- day of month
        'd' -> digits ZeroPadding 2
        'e' -> digits SpacePadding 2

        -- week of year
        'V' -> digits ZeroPadding 2
        'U' -> digits ZeroPadding 2
        'W' -> digits ZeroPadding 2

        -- day of week
        'u' -> oneOf $ map (:[]) ['1'..'7']
        'a' -> oneOf (map snd (wDays l))
        'A' -> oneOf (map fst (wDays l))
        'w' -> oneOf $ map (:[]) ['0'..'6']

        -- day of year
        'j' -> digits ZeroPadding 3

        -- dayhalf of day (i.e. AM or PM)
        'P' -> oneOf (let (am,pm) = amPm l in [am, pm])
        'p' -> oneOf (let (am,pm) = amPm l in [am, pm])

        -- hour of day (i.e. 24h)
        'H' -> digits ZeroPadding 2
        'k' -> digits SpacePadding 2

        -- hour of dayhalf (i.e. 12h)
        'I' -> digits ZeroPadding 2
        'l' -> digits SpacePadding 2

        -- minute of hour
        'M' -> digits ZeroPadding 2

        -- second of minute
        'S' -> digits ZeroPadding 2

        -- picosecond of second
        'q' -> digits ZeroPadding 12
        'Q' -> liftA2 (:) (char '.') (munch isDigit) <++ return ""

        -- time zone
        'z' -> numericTZ
        'Z' -> munch1 isAlpha <++
             numericTZ

        -- seconds since epoch
        's' -> (char '-' >> fmap ('-':) (munch1 isDigit))
             <++ munch1 isDigit

        _   -> fail $ "Unknown format character: " ++ show c

timeSubstituteTimeSpecifier :: TimeLocale -> Char -> Maybe String
timeSubstituteTimeSpecifier l 'c' = Just $ dateTimeFmt l
timeSubstituteTimeSpecifier _ 'R' = Just "%H:%M"
timeSubstituteTimeSpecifier _ 'T' = Just "%H:%M:%S"
timeSubstituteTimeSpecifier l 'X' = Just $ timeFmt l
timeSubstituteTimeSpecifier l 'r' = Just $ time12Fmt l
timeSubstituteTimeSpecifier _ 'D' = Just "%m/%d/%y"
timeSubstituteTimeSpecifier _ 'F' = Just "%Y-%m-%d"
timeSubstituteTimeSpecifier l 'x' = Just $ dateFmt l
timeSubstituteTimeSpecifier _ 'h' = Just "%b"
timeSubstituteTimeSpecifier _  _ = Nothing

durationParseTimeSpecifier :: TimeLocale -> Maybe ParseNumericPadding -> Char -> ReadP String
durationParseTimeSpecifier _ mpad c = let
    padopt = parsePaddedSignedDigits $ fromMaybe NoPadding mpad
    in case c of
        'y' -> padopt 1
        'b' -> padopt 1
        'B' -> padopt 2
        'w' -> padopt 1
        'd' -> padopt 1
        'D' -> padopt 1
        'h' -> padopt 1
        'H' -> padopt 2
        'm' -> padopt 1
        'M' -> padopt 2
        's' -> parseSignedDecimal
        'S' -> parseSignedDecimal
        _   -> fail $ "Unknown format character: " ++ show c
