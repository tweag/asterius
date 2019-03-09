{-# OPTIONS -fno-warn-orphans #-}
module Data.Time.Format.Parse
    (
    -- * UNIX-style parsing
    parseTimeM, parseTimeOrError, readSTime, readPTime,
    parseTime, readTime, readsTime,
    ParseTime(),
    -- * Locale
    module Data.Time.Format.Locale
    ) where

import Data.Proxy
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail
import Prelude hiding (fail)
#endif
import Data.Char
import Data.Time.Format.Locale
import Text.ParserCombinators.ReadP hiding (char, string)
import Data.Time.Clock.Internal.UniversalTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Calendar.Days
import Data.Time.LocalTime.Internal.TimeZone
import Data.Time.LocalTime.Internal.TimeOfDay
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.Internal.ZonedTime
import Data.Time.Format.Parse.Class
import Data.Time.Format.Parse.Instances()

-- | Parses a time value given a format string.
-- Supports the same %-codes as 'formatTime', including @%-@, @%_@ and @%0@ modifiers, however padding widths are not supported.
-- Case is not significant in the input string.
-- Some variations in the input are accepted:
--
-- [@%z@] accepts any of @±HHMM@ or @±HH:MM@.
--
-- [@%Z@] accepts any string of letters, or any of the formats accepted by @%z@.
--
-- [@%0Y@] accepts exactly four digits.
--
-- [@%0G@] accepts exactly four digits.
--
-- [@%0C@] accepts exactly two digits.
--
-- [@%0f@] accepts exactly two digits.
--
-- For example, to parse a date in YYYY-MM-DD format, while allowing the month
-- and date to have optional leading zeros (notice the @-@ modifier used for @%m@
-- and @%d@):
--
-- > Prelude Data.Time> parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04" :: Maybe Day
-- > Just 2010-03-04
--
parseTimeM :: (
#if MIN_VERSION_base(4,9,0)
    MonadFail m
#else
    Monad m
#endif
    ,ParseTime t) =>
             Bool       -- ^ Accept leading and trailing whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> m t    -- ^ Return the time value, or fail if the input could
                        -- not be parsed using the given format.
parseTimeM acceptWS l fmt s = case parseTimeList acceptWS l fmt s of
    [t] -> return t
    []  -> fail $ "parseTimeM: no parse of " ++ show s
    _   -> fail $ "parseTimeM: multiple parses of " ++ show s

-- | Parse a time value given a format string. Fails if the input could
-- not be parsed using the given format. See 'parseTimeM' for details.
parseTimeOrError :: ParseTime t =>
             Bool       -- ^ Accept leading and trailing whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> t          -- ^ The time value.
parseTimeOrError acceptWS l fmt s = case parseTimeList acceptWS l fmt s of
    [t] -> t
    []  -> error $ "parseTimeOrError: no parse of " ++ show s
    _   -> error $ "parseTimeOrError: multiple parses of " ++ show s

parseTimeList :: ParseTime t =>
             Bool       -- ^ Accept leading and trailing whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> String     -- ^ Input string.
          -> [t]
parseTimeList False l fmt s = [t | (t,"") <- readSTime False l fmt s]
parseTimeList True l fmt s = [t | (t,r) <- readSTime True l fmt s, all isSpace r]

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readSTime :: ParseTime t =>
             Bool       -- ^ Accept leading whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadS t
readSTime acceptWS l f = readP_to_S (readPTime acceptWS l f)

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readPTime :: ParseTime t =>
             Bool       -- ^ Accept leading whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadP t
readPTime False l f = readPOnlyTime l f
readPTime True l f = (skipSpaces >> readPOnlyTime l f) <++ readPOnlyTime l f

readPOnlyTime' :: ParseTime t => proxy t -> TimeLocale -> String -> ReadP t
readPOnlyTime' pt l f = do
    pairs <- parseSpecifiers pt l f
    case buildTime l pairs of
        Just t -> return t
        Nothing -> pfail

-- | Parse a time value given a format string (without allowing leading whitespace).  See 'parseTimeM' for details.
readPOnlyTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadP t
readPOnlyTime = readPOnlyTime' Proxy

{-# DEPRECATED parseTime "use \"parseTimeM True\" instead" #-}
parseTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> Maybe t    -- ^ The time value, or 'Nothing' if the input could
                        -- not be parsed using the given format.
parseTime = parseTimeM True

{-# DEPRECATED readTime "use \"parseTimeOrError True\" instead" #-}
readTime :: ParseTime t =>
            TimeLocale -- ^ Time locale.
         -> String     -- ^ Format string.
         -> String     -- ^ Input string.
         -> t          -- ^ The time value.
readTime = parseTimeOrError True

{-# DEPRECATED readsTime "use \"readSTime True\" instead" #-}
readsTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadS t
readsTime = readSTime True

-- * Read instances for time package types

instance Read Day where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d"

instance Read TimeOfDay where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%H:%M:%S%Q"

instance Read LocalTime where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

instance Read TimeZone where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Z"

instance Read ZonedTime where
    readsPrec n = readParen False $ \s ->
        [(ZonedTime t z, r2) | (t,r1) <- readsPrec n s, (z,r2) <- readsPrec n r1]

instance Read UTCTime where
    readsPrec n s = [ (zonedTimeToUTC t, r) | (t,r) <- readsPrec n s ]

instance Read UniversalTime where
    readsPrec n s = [ (localTimeToUT1 0 t, r) | (t,r) <- readsPrec n s ]
