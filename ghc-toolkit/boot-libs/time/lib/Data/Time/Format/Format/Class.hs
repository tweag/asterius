module Data.Time.Format.Format.Class
    (
        -- * Formatting
        formatTime,
        FormatNumericPadding,
        FormatOptions(..),
        FormatTime(..),
        ShowPadded,PadOption,
        formatGeneral,formatString,formatNumber,formatNumberStd,
        showPaddedFixed,showPaddedFixedFraction,
        quotBy,remBy,
    )
    where

import Data.Char
import Data.Maybe
import Data.Fixed
import Data.Time.Calendar.Private
import Data.Time.Format.Locale

type FormatNumericPadding = Maybe Char

data FormatOptions = MkFormatOptions {
    foLocale :: TimeLocale,
    foPadding :: Maybe FormatNumericPadding,
    foWidth :: Maybe Int
}

-- <http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html>
class FormatTime t where
    -- | @since 1.9.1
    formatCharacter :: Bool -> Char -> Maybe (FormatOptions -> t -> String)


-- the weird UNIX logic is here
getPadOption :: Bool -> Bool -> Int -> Char -> Maybe FormatNumericPadding -> Maybe Int -> PadOption
getPadOption trunc fdef idef cdef mnpad mi = let
    c = case mnpad of
        Just (Just c') -> c'
        Just Nothing -> ' '
        _ -> cdef
    i = case mi of
        Just i' -> case mnpad of
            Just Nothing -> i'
            _ -> if trunc then i' else max i' idef
        Nothing -> idef
    f = case mi of
        Just _ -> True
        Nothing -> case mnpad of
            Nothing -> fdef
            Just Nothing -> False
            Just (Just _) -> True
    in if f then Pad i c else NoPad

formatGeneral :: Bool -> Bool -> Int -> Char -> (TimeLocale -> PadOption -> t -> String) -> (FormatOptions -> t -> String)
formatGeneral trunc fdef idef cdef ff fo = ff (foLocale fo) $ getPadOption trunc fdef idef cdef (foPadding fo) (foWidth fo)

formatString :: (TimeLocale -> t -> String) -> (FormatOptions -> t -> String)
formatString ff = formatGeneral False False 1 ' ' $ \locale pado -> showPadded pado . ff locale

formatNumber :: (ShowPadded i) => Bool -> Int -> Char -> (t -> i) -> (FormatOptions -> t -> String)
formatNumber fdef idef cdef ff = formatGeneral False fdef idef cdef $ \_ pado -> showPaddedNum pado . ff

formatNumberStd :: Int -> (t -> Integer) -> (FormatOptions -> t -> String)
formatNumberStd n = formatNumber False n '0'

showPaddedFixed :: HasResolution a => PadOption -> PadOption -> Fixed a -> String
showPaddedFixed padn padf x | x < 0 = '-' : showPaddedFixed padn padf (negate x)
showPaddedFixed padn padf x = let
    ns = showPaddedNum padn $ (floor x :: Integer)
    fs = showPaddedFixedFraction padf x
    ds = if null fs then "" else "."
    in ns ++ ds ++ fs

showPaddedFixedFraction :: HasResolution a => PadOption -> Fixed a -> String
showPaddedFixedFraction pado x = let
    digits = dropWhile (=='.') $ dropWhile (/='.') $ showFixed True x
    n = length digits
    in case pado of
        NoPad -> digits
        Pad i c -> if i < n
            then take i digits
            else digits ++ replicate (i - n) c


-- | Substitute various time-related information for each %-code in the string, as per 'formatCharacter'.
--
-- The general form is @%\<modifier\>\<width\>\<alternate\>\<specifier\>@, where @\<modifier\>@, @\<width\>@, and @\<alternate\>@ are optional.
--
-- == @\<modifier\>@
-- glibc-style modifiers can be used before the specifier (here marked as @z@):
--
-- [@%-z@] no padding
--
-- [@%_z@] pad with spaces
--
-- [@%0z@] pad with zeros
--
-- [@%^z@] convert to upper case
--
-- [@%#z@] convert to lower case (consistently, unlike glibc)
--
-- == @\<width\>@
-- Width digits can also be used after any modifiers and before the specifier (here marked as @z@), for example:
--
-- [@%4z@] pad to 4 characters (with default padding character)
--
-- [@%_12z@] pad with spaces to 12 characters
--
-- == @\<alternate\>@
-- An optional @E@ character indicates an alternate formatting. Currently this only affects @%Z@ and @%z@.
--
-- [@%Ez@] alternate formatting
--
-- == @\<specifier\>@
--
-- For all types (note these three are done by 'formatTime', not by 'formatCharacter'):
--
-- [@%%@] @%@
--
-- [@%t@] tab
--
-- [@%n@] newline
--
-- === 'TimeZone'
-- For 'TimeZone' (and 'ZonedTime' and 'UTCTime'):
--
-- [@%z@] timezone offset in the format @±HHMM@
--
-- [@%Ez@] timezone offset in the format @±HH:MM@
--
-- [@%Z@] timezone name (or else offset in the format @±HHMM@)
--
-- [@%EZ@] timezone name (or else offset in the format @±HH:MM@)
--
-- === 'LocalTime'
-- For 'LocalTime' (and 'ZonedTime' and 'UTCTime' and 'UniversalTime'):
--
-- [@%c@] as 'dateTimeFmt' @locale@ (e.g. @%a %b %e %H:%M:%S %Z %Y@)
--
-- === 'TimeOfDay'
-- For 'TimeOfDay' (and 'LocalTime' and 'ZonedTime' and 'UTCTime' and 'UniversalTime'):
--
-- [@%R@] same as @%H:%M@
--
-- [@%T@] same as @%H:%M:%S@
--
-- [@%X@] as 'timeFmt' @locale@ (e.g. @%H:%M:%S@)
--
-- [@%r@] as 'time12Fmt' @locale@ (e.g. @%I:%M:%S %p@)
--
-- [@%P@] day-half of day from ('amPm' @locale@), converted to lowercase, @am@, @pm@
--
-- [@%p@] day-half of day from ('amPm' @locale@), @AM@, @PM@
--
-- [@%H@] hour of day (24-hour), 0-padded to two chars, @00@ - @23@
--
-- [@%k@] hour of day (24-hour), space-padded to two chars, @ 0@ - @23@
--
-- [@%I@] hour of day-half (12-hour), 0-padded to two chars, @01@ - @12@
--
-- [@%l@] hour of day-half (12-hour), space-padded to two chars, @ 1@ - @12@
--
-- [@%M@] minute of hour, 0-padded to two chars, @00@ - @59@
--
-- [@%S@] second of minute (without decimal part), 0-padded to two chars, @00@ - @60@
--
-- [@%q@] picosecond of second, 0-padded to twelve chars, @000000000000@ - @999999999999@.
--
-- [@%Q@] decimal point and fraction of second, up to 12 second decimals, without trailing zeros.
-- For a whole number of seconds, @%Q@ omits the decimal point unless padding is specified.
--
-- === 'UTCTime' and 'ZonedTime'
-- For 'UTCTime' and 'ZonedTime':
--
-- [@%s@] number of whole seconds since the Unix epoch. For times before
-- the Unix epoch, this is a negative number. Note that in @%s.%q@ and @%s%Q@
-- the decimals are positive, not negative. For example, 0.9 seconds
-- before the Unix epoch is formatted as @-1.1@ with @%s%Q@.
--
-- === 'DayOfWeek'
-- For 'DayOfWeek' (and 'Day' and 'LocalTime' and 'ZonedTime' and 'UTCTime' and 'UniversalTime'):
--
-- [@%u@] day of week number for Week Date format, @1@ (= Monday) - @7@ (= Sunday)
--
-- [@%w@] day of week number, @0@ (= Sunday) - @6@ (= Saturday)
--
-- [@%a@] day of week, short form ('snd' from 'wDays' @locale@), @Sun@ - @Sat@
--
-- [@%A@] day of week, long form ('fst' from 'wDays' @locale@), @Sunday@ - @Saturday@
--
-- === 'Day'
-- For 'Day' (and 'LocalTime' and 'ZonedTime' and 'UTCTime' and 'UniversalTime'):
--
-- [@%D@] same as @%m\/%d\/%y@
--
-- [@%F@] same as @%Y-%m-%d@
--
-- [@%x@] as 'dateFmt' @locale@ (e.g. @%m\/%d\/%y@)
--
-- [@%Y@] year, no padding. Note @%0Y@ and @%_Y@ pad to four chars
--
-- [@%y@] year of century, 0-padded to two chars, @00@ - @99@
--
-- [@%C@] century, no padding. Note @%0C@ and @%_C@ pad to two chars
--
-- [@%B@] month name, long form ('fst' from 'months' @locale@), @January@ - @December@
--
-- [@%b@, @%h@] month name, short form ('snd' from 'months' @locale@), @Jan@ - @Dec@
--
-- [@%m@] month of year, 0-padded to two chars, @01@ - @12@
--
-- [@%d@] day of month, 0-padded to two chars, @01@ - @31@
--
-- [@%e@] day of month, space-padded to two chars,  @ 1@ - @31@
--
-- [@%j@] day of year, 0-padded to three chars, @001@ - @366@
--
-- [@%f@] century for Week Date format, no padding. Note @%0f@ and @%_f@ pad to two chars
--
-- [@%V@] week of year for Week Date format, 0-padded to two chars, @01@ - @53@
--
-- [@%U@] week of year where weeks start on Sunday (as 'sundayStartWeek'), 0-padded to two chars, @00@ - @53@
--
-- [@%W@] week of year where weeks start on Monday (as 'mondayStartWeek'), 0-padded to two chars, @00@ - @53@
--
-- == Duration types
-- The specifiers for 'DiffTime', 'NominalDiffTime', 'CalendarDiffDays', and 'CalendarDiffTime' are semantically
-- separate from the other types.
-- Specifiers on negative time differences will generally be negative (think 'rem' rather than 'mod').
--
-- === 'NominalDiffTime' and 'DiffTime'
-- Note that a "minute" of 'DiffTime' is simply 60 SI seconds, rather than a minute of civil time.
-- Use 'NominalDiffTime' to work with civil time, ignoring any leap seconds.
--
-- For 'NominalDiffTime' and 'DiffTime':
--
-- [@%w@] total whole weeks
--
-- [@%d@] total whole days
--
-- [@%D@] whole days of week
--
-- [@%h@] total whole hours
--
-- [@%H@] whole hours of day
--
-- [@%m@] total whole minutes
--
-- [@%M@] whole minutes of hour
--
-- [@%s@] total whole seconds
--
-- [@%Es@] total seconds, with decimal point and up to \<width\> (default 12) decimal places, without trailing zeros.
-- For a whole number of seconds, @%Es@ omits the decimal point unless padding is specified.
--
-- [@%0Es@] total seconds, with decimal point and \<width\> (default 12) decimal places.
--
-- [@%S@] whole seconds of minute
--
-- [@%ES@] seconds of minute, with decimal point and up to \<width\> (default 12) decimal places, without trailing zeros.
-- For a whole number of seconds, @%ES@ omits the decimal point unless padding is specified.
--
-- [@%0ES@] seconds of minute as two digits, with decimal point and \<width\> (default 12) decimal places.
--
-- === 'CalendarDiffDays'
-- For 'CalendarDiffDays' (and 'CalendarDiffTime'):
--
-- [@%y@] total years
--
-- [@%b@] total months
--
-- [@%B@] months of year
--
-- [@%w@] total weeks, not including months
--
-- [@%d@] total days, not including months
--
-- [@%D@] days of week
--
-- === 'CalendarDiffTime'
-- For 'CalendarDiffTime':
--
-- [@%h@] total hours, not including months
--
-- [@%H@] hours of day
--
-- [@%m@] total minutes, not including months
--
-- [@%M@] minutes of hour
--
-- [@%s@] total whole seconds, not including months
--
-- [@%Es@] total seconds, not including months, with decimal point and up to \<width\> (default 12) decimal places, without trailing zeros.
-- For a whole number of seconds, @%Es@ omits the decimal point unless padding is specified.
--
-- [@%0Es@] total seconds, not including months, with decimal point and \<width\> (default 12) decimal places.
--
-- [@%S@] whole seconds of minute
--
-- [@%ES@] seconds of minute, with decimal point and up to \<width\> (default 12) decimal places, without trailing zeros.
-- For a whole number of seconds, @%ES@ omits the decimal point unless padding is specified.
--
-- [@%0ES@] seconds of minute as two digits, with decimal point and \<width\> (default 12) decimal places.
formatTime :: (FormatTime t) => TimeLocale -> String -> t -> String
formatTime _ [] _ = ""
formatTime locale ('%':cs) t = case formatTime1 locale cs t of
    Just result -> result
    Nothing -> '%':(formatTime locale cs t)
formatTime locale (c:cs) t = c:(formatTime locale cs t)

formatTime1 :: (FormatTime t) => TimeLocale -> String -> t -> Maybe String
formatTime1 locale ('_':cs) t = formatTime2 locale id (Just (Just ' ')) cs t
formatTime1 locale ('-':cs) t = formatTime2 locale id (Just Nothing) cs t
formatTime1 locale ('0':cs) t = formatTime2 locale id (Just (Just '0')) cs t
formatTime1 locale ('^':cs) t = formatTime2 locale (fmap toUpper) Nothing cs t
formatTime1 locale ('#':cs) t = formatTime2 locale (fmap toLower) Nothing cs t
formatTime1 locale cs t = formatTime2 locale id Nothing cs t

getDigit :: Char -> Maybe Int
getDigit c | c < '0' = Nothing
getDigit c | c > '9' = Nothing
getDigit c = Just $ (ord c) - (ord '0')

pullNumber :: Maybe Int -> String -> (Maybe Int,String)
pullNumber mx [] = (mx,[])
pullNumber mx s@(c:cs) = case getDigit c of
    Just i -> pullNumber (Just $ (fromMaybe 0 mx)*10+i) cs
    Nothing -> (mx,s)

formatTime2 :: (FormatTime t) => TimeLocale -> (String -> String) -> Maybe FormatNumericPadding -> String -> t -> Maybe String
formatTime2 locale recase mpad cs t = let
    (mwidth,rest) = pullNumber Nothing cs
    in formatTime3 locale recase mpad mwidth rest t

formatTime3 :: (FormatTime t) => TimeLocale -> (String -> String) -> Maybe FormatNumericPadding -> Maybe Int -> String -> t -> Maybe String
formatTime3 locale recase mpad mwidth ('E':cs) = formatTime4 True recase (MkFormatOptions locale mpad mwidth) cs
formatTime3 locale recase mpad mwidth cs = formatTime4 False recase (MkFormatOptions locale mpad mwidth) cs

formatTime4 :: (FormatTime t) => Bool -> (String -> String) -> FormatOptions -> String -> t -> Maybe String
formatTime4 alt recase fo (c:cs) t = Just $ (recase (formatChar alt c fo t)) ++ (formatTime (foLocale fo) cs t)
formatTime4 _alt _recase _fo [] _t = Nothing

formatChar :: (FormatTime t) => Bool -> Char -> FormatOptions -> t -> String
formatChar _ '%' = formatString $ \_ _ -> "%"
formatChar _ 't' = formatString $ \_ _ -> "\t"
formatChar _ 'n' = formatString $ \_ _ -> "\n"
formatChar alt c = case formatCharacter alt c of
    Just f -> f
    _ -> \_ _ -> ""
