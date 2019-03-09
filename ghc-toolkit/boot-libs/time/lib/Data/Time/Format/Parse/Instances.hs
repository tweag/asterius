{-# OPTIONS -fno-warn-orphans #-}
module Data.Time.Format.Parse.Instances() where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>),(<*>))
#endif
import Data.Char
import Data.Fixed
import Data.List
import Data.Ratio
import Data.Traversable
import Text.Read(readMaybe)
import Data.Time.Clock.Internal.DiffTime
import Data.Time.Clock.Internal.NominalDiffTime
import Data.Time.Clock.Internal.UniversalTime
import Data.Time.Clock.POSIX
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.CalendarDiffDays
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.Private(clipValid)
import Data.Time.LocalTime.Internal.CalendarDiffTime
import Data.Time.LocalTime.Internal.TimeZone
import Data.Time.LocalTime.Internal.TimeOfDay
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.Internal.ZonedTime
import Data.Time.Format.Locale
import Data.Time.Format.Parse.Class

data DayComponent = Century Integer -- century of all years
                  | CenturyYear Integer -- 0-99, last two digits of both real years and week years
                  | YearMonth Int -- 1-12
                  | MonthDay Int -- 1-31
                  | YearDay Int -- 1-366
                  | WeekDay Int -- 1-7 (mon-sun)
                  | YearWeek WeekType Int -- 1-53 or 0-53

data WeekType = ISOWeek | SundayWeek | MondayWeek

instance ParseTime Day where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l = let

        -- 'Nothing' indicates a parse failure,
        -- while 'Just []' means no information
        f :: Char -> String -> Maybe [DayComponent]
        f c x = let
            ra :: (Read a) => Maybe a
            ra = readMaybe x

            zeroBasedListIndex :: [String] -> Maybe Int
            zeroBasedListIndex ss = elemIndex (map toUpper x) $ fmap (map toUpper) ss

            oneBasedListIndex :: [String] -> Maybe Int
            oneBasedListIndex ss = do
                index <- zeroBasedListIndex ss
                return $ 1 + index

            in case c of
            -- %C: century (all but the last two digits of the year), 00 - 99
            'C' -> do
                a <- ra
                return [Century a]
            -- %f century (all but the last two digits of the year), 00 - 99
            'f' -> do
                a <- ra
                return [Century a]
            -- %Y: year
            'Y' -> do
                a <- ra
                return [Century (a `div` 100), CenturyYear (a `mod` 100)]
            -- %G: year for Week Date format
            'G' -> do
                a <- ra
                return [Century (a `div` 100), CenturyYear (a `mod` 100)]
            -- %y: last two digits of year, 00 - 99
            'y' -> do
                a <- ra
                return [CenturyYear a]
            -- %g: last two digits of year for Week Date format, 00 - 99
            'g' -> do
                a <- ra
                return [CenturyYear a]
            -- %B: month name, long form (fst from months locale), January - December
            'B' -> do
                a <- oneBasedListIndex $ fmap fst $ months l
                return [YearMonth a]
            -- %b: month name, short form (snd from months locale), Jan - Dec
            'b' -> do
                a <- oneBasedListIndex $ fmap snd $ months l
                return [YearMonth a]
            -- %m: month of year, leading 0 as needed, 01 - 12
            'm' -> do
                raw <- ra
                a <- clipValid 1 12 raw
                return [YearMonth a]
            -- %d: day of month, leading 0 as needed, 01 - 31
            'd' -> do
                raw <- ra
                a <- clipValid 1 31 raw
                return [MonthDay a]
            -- %e: day of month, leading space as needed, 1 - 31
            'e' -> do
                raw <- ra
                a <- clipValid 1 31 raw
                return [MonthDay a]
            -- %V: week for Week Date format, 01 - 53
            'V' -> do
                raw <- ra
                a <- clipValid 1 53 raw
                return [YearWeek ISOWeek a]
            -- %U: week number of year, where weeks start on Sunday (as sundayStartWeek), 00 - 53
            'U' -> do
                raw <- ra
                a <- clipValid 0 53 raw
                return [YearWeek SundayWeek a]
            -- %W: week number of year, where weeks start on Monday (as mondayStartWeek), 00 - 53
            'W' -> do
                raw <- ra
                a <- clipValid 0 53 raw
                return [YearWeek MondayWeek a]
            -- %u: day for Week Date format, 1 - 7
            'u' -> do
                raw <- ra
                a <- clipValid 1 7 raw
                return [WeekDay a]
            -- %a: day of week, short form (snd from wDays locale), Sun - Sat
            'a' -> do
                a' <- zeroBasedListIndex $ fmap snd $ wDays l
                let a = if a' == 0 then 7 else a'
                return [WeekDay a]
            -- %A: day of week, long form (fst from wDays locale), Sunday - Saturday
            'A' -> do
                a' <- zeroBasedListIndex $ fmap fst $ wDays l
                let a = if a' == 0 then 7 else a'
                return [WeekDay a]
            -- %w: day of week number, 0 (= Sunday) - 6 (= Saturday)
            'w' -> do
                raw <- ra
                a' <- clipValid 0 6 raw
                let a = if a' == 0 then 7 else a'
                return [WeekDay a]
            -- %j: day of year for Ordinal Date format, 001 - 366
            'j' -> do
                raw <- ra
                a <- clipValid 1 366 raw
                return [YearDay a]
            -- unrecognised, pass on to other parsers
            _   -> return []

        buildDay :: [DayComponent] -> Maybe Day
        buildDay cs = let
            safeLast x xs = last (x:xs)
            y = let
                d = safeLast 70 [x | CenturyYear x <- cs]
                c = safeLast (if d >= 69 then 19 else 20) [x | Century x <- cs]
                in 100 * c + d
            rest (YearMonth m:_) = let
                d = safeLast 1 [x | MonthDay x <- cs]
                in fromGregorianValid y m d
            rest (YearDay d:_) = fromOrdinalDateValid y d
            rest (YearWeek wt w:_) = let
                d = safeLast 4 [x | WeekDay x <- cs]
                in case wt of
                    ISOWeek    -> fromWeekDateValid y w d
                    SundayWeek -> fromSundayStartWeekValid y w (d `mod` 7)
                    MondayWeek -> fromMondayStartWeekValid y w d
            rest (_:xs)        = rest xs
            rest []            = rest [YearMonth 1]

            in rest cs

        in \pairs -> do
            components <- for pairs $ \(c,x) -> f c x
            buildDay $ concat components

mfoldl :: (Monad m) => (a -> b -> m a) -> m a -> [b] -> m a
mfoldl f = let
    mf ma b = do
        a <- ma
        f a b
    in foldl mf

instance ParseTime TimeOfDay where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l = let
        f t@(TimeOfDay h m s) (c,x) = let
            ra :: (Read a) => Maybe a
            ra = readMaybe x

            getAmPm = let
                upx = map toUpper x
                (amStr,pmStr) = amPm l
                in if upx == amStr
                    then Just $ TimeOfDay (h `mod` 12) m s
                    else if upx == pmStr
                    then Just $ TimeOfDay (if h < 12 then h + 12 else h) m s
                    else Nothing

            in case c of
                'P' -> getAmPm
                'p' -> getAmPm
                'H' -> do
                    raw <- ra
                    a <- clipValid 0 23 raw
                    return $ TimeOfDay a m s
                'I' -> do
                    raw <- ra
                    a <- clipValid 1 12 raw
                    return $ TimeOfDay a m s
                'k' -> do
                    raw <- ra
                    a <- clipValid 0 23 raw
                    return $ TimeOfDay a m s
                'l' -> do
                    raw <- ra
                    a <- clipValid 1 12 raw
                    return $ TimeOfDay a m s
                'M' -> do
                    raw <- ra
                    a <- clipValid 0 59 raw
                    return $ TimeOfDay h a s
                'S' -> do
                    raw <- ra
                    a <- clipValid 0 60 raw
                    return $ TimeOfDay h m (fromInteger a)
                'q' -> do
                    a <- ra
                    return $ TimeOfDay h m (mkPico (floor s) a)
                'Q' -> if null x then Just t else do
                    ps <- readMaybe $ take 12 $ rpad 12 '0' $ drop 1 x
                    return $ TimeOfDay h m (mkPico (floor s) ps)
                _   -> Just t

        in mfoldl f (Just midnight)

rpad :: Int -> a -> [a] -> [a]
rpad n c xs = xs ++ replicate (n - length xs) c

mkPico :: Integer -> Integer -> Pico
mkPico i f = fromInteger i + fromRational (f % 1000000000000)

instance ParseTime LocalTime where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l xs = LocalTime <$> (buildTime l xs) <*> (buildTime l xs)

enumDiff :: (Enum a) => a -> a -> Int
enumDiff a b = (fromEnum a) - (fromEnum b)

getMilZoneHours :: Char -> Maybe Int
getMilZoneHours c | c < 'A' = Nothing
getMilZoneHours c | c <= 'I' = Just $ 1 + enumDiff c 'A'
getMilZoneHours 'J' = Nothing
getMilZoneHours c | c <= 'M' = Just $ 10 + enumDiff c 'K'
getMilZoneHours c | c <= 'Y' = Just $ (enumDiff 'N' c) - 1
getMilZoneHours 'Z' = Just 0
getMilZoneHours _ = Nothing

getMilZone :: Char -> Maybe TimeZone
getMilZone c = let
    yc = toUpper c
    in do
        hours <- getMilZoneHours yc
        return $ TimeZone (hours * 60) False [yc]

getKnownTimeZone :: TimeLocale -> String -> Maybe TimeZone
getKnownTimeZone locale x = find (\tz -> map toUpper x == timeZoneName tz) (knownTimeZones locale)

instance ParseTime TimeZone where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l = let
        f :: Char -> String -> TimeZone -> Maybe TimeZone
        f 'z' str (TimeZone _ dst name) | Just offset <- readTzOffset str = Just $ TimeZone offset dst name
        f 'z' _ _ = Nothing
        f 'Z' str _ | Just offset <- readTzOffset str = Just $ TimeZone offset False ""
        f 'Z' str _ | Just zone <- getKnownTimeZone l str = Just zone
        f 'Z' "UTC" _ = Just utc
        f 'Z' [c] _ | Just zone <- getMilZone c = Just zone
        f 'Z' _ _ = Nothing
        f _ _ tz = Just tz
        in foldl (\mt (c,s) -> mt >>= f c s) (Just $ minutesToTimeZone 0)

readTzOffset :: String -> Maybe Int
readTzOffset str = let

    getSign '+' = Just 1
    getSign '-' = Just (-1)
    getSign _ = Nothing

    calc s h1 h2 m1 m2 = do
        sign <- getSign s
        h <- readMaybe [h1,h2]
        m <- readMaybe [m1,m2]
        return $ sign * (60 * h + m)

    in case str of
        (s:h1:h2:':':m1:m2:[]) -> calc s h1 h2 m1 m2
        (s:h1:h2:m1:m2:[]) -> calc s h1 h2 m1 m2
        _ -> Nothing

instance ParseTime ZonedTime where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l xs = let
        f (ZonedTime (LocalTime _ tod) z) ('s',x) = do
            a <- readMaybe x
            let
                s = fromInteger a
                (_,ps) = properFraction (todSec tod) :: (Integer,Pico)
                s' = s + fromRational (toRational ps)
            return $ utcToZonedTime z (posixSecondsToUTCTime s')
        f t _ = Just t
        in mfoldl f (ZonedTime <$> (buildTime l xs) <*> (buildTime l xs)) xs

instance ParseTime UTCTime where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l xs = zonedTimeToUTC <$> buildTime l xs

instance ParseTime UniversalTime where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l xs = localTimeToUT1 0 <$> buildTime l xs

buildTimeMonths :: [(Char,String)] -> Maybe Integer
buildTimeMonths xs = do
    tt <- for xs $ \(c,s) -> case c of
        'y' -> fmap ((*) 12) $ readMaybe s
        'b' -> readMaybe s
        'B' -> readMaybe s
        _ -> return 0
    return $ sum tt

buildTimeDays :: [(Char,String)] -> Maybe Integer
buildTimeDays xs = do
    tt <- for xs $ \(c,s) -> case c of
        'w' -> fmap ((*) 7) $ readMaybe s
        'd' -> readMaybe s
        'D' -> readMaybe s
        _ -> return 0
    return $ sum tt

buildTimeSeconds :: [(Char,String)] -> Maybe Pico
buildTimeSeconds xs = do
    tt <- for xs $ \(c,s) -> let
        readInt :: Integer -> Maybe Pico
        readInt t = do
            i <- readMaybe s
            return $ fromInteger $ i * t
        in case c of
            'h' -> readInt 3600
            'H' -> readInt 3600
            'm' -> readInt 60
            'M' -> readInt 60
            's' -> readMaybe s
            'S' -> readMaybe s
            _ -> return 0
    return $ sum tt

instance ParseTime NominalDiffTime where
    parseTimeSpecifier _ = durationParseTimeSpecifier
    buildTime _ xs = do
        dd <- buildTimeDays xs
        tt <- buildTimeSeconds xs
        return $ (fromInteger dd * 86400) + realToFrac tt

instance ParseTime DiffTime where
    parseTimeSpecifier _ = durationParseTimeSpecifier
    buildTime _ xs = do
        dd <- buildTimeDays xs
        tt <- buildTimeSeconds xs
        return $ (fromInteger dd * 86400) + realToFrac tt

instance ParseTime CalendarDiffDays where
    parseTimeSpecifier _ = durationParseTimeSpecifier
    buildTime _ xs = do
        mm <- buildTimeMonths xs
        dd <- buildTimeDays xs
        return $ CalendarDiffDays mm dd

instance ParseTime CalendarDiffTime where
    parseTimeSpecifier _ = durationParseTimeSpecifier
    buildTime locale xs = do
        mm <- buildTimeMonths xs
        tt <- buildTime locale xs
        return $ CalendarDiffTime mm tt
