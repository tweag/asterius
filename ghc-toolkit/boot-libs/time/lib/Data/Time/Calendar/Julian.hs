module Data.Time.Calendar.Julian
(
    module Data.Time.Calendar.JulianYearDay,

    toJulian,fromJulian,fromJulianValid,showJulian,julianMonthLength,

    -- calendrical arithmetic
    -- e.g. "one month after March 31st"
    addJulianMonthsClip,addJulianMonthsRollOver,
    addJulianYearsClip,addJulianYearsRollOver,
    addJulianDurationClip,addJulianDurationRollOver,
    diffJulianDurationClip,diffJulianDurationRollOver,
) where

import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.JulianYearDay
import Data.Time.Calendar.Days
import Data.Time.Calendar.CalendarDiffDays
import Data.Time.Calendar.Private

-- | Convert to proleptic Julian calendar. First element of result is year, second month number (1-12), third day (1-31).
toJulian :: Day -> (Integer,Int,Int)
toJulian date = (year,month,day) where
    (year,yd) = toJulianYearAndDay date
    (month,day) = dayOfYearToMonthAndDay (isJulianLeapYear year) yd

-- | Convert from proleptic Julian calendar. First argument is year, second month number (1-12), third day (1-31).
-- Invalid values will be clipped to the correct range, month first, then day.
fromJulian :: Integer -> Int -> Int -> Day
fromJulian year month day = fromJulianYearAndDay year (monthAndDayToDayOfYear (isJulianLeapYear year) month day)

-- | Convert from proleptic Julian calendar. First argument is year, second month number (1-12), third day (1-31).
-- Invalid values will return Nothing.
fromJulianValid :: Integer -> Int -> Int -> Maybe Day
fromJulianValid year month day = do
    doy <- monthAndDayToDayOfYearValid (isJulianLeapYear year) month day
    fromJulianYearAndDayValid year doy

-- | Show in ISO 8601 format (yyyy-mm-dd)
showJulian :: Day -> String
showJulian date = (show4 y) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d) where
    (y,m,d) = toJulian date

-- | The number of days in a given month according to the proleptic Julian calendar. First argument is year, second is month.
julianMonthLength :: Integer -> Int -> Int
julianMonthLength year = monthLength (isJulianLeapYear year)

rolloverMonths :: (Integer,Integer) -> (Integer,Int)
rolloverMonths (y,m) = (y + (div (m - 1) 12),fromIntegral (mod (m - 1) 12) + 1)

addJulianMonths :: Integer -> Day -> (Integer,Int,Int)
addJulianMonths n day = (y',m',d) where
    (y,m,d) = toJulian day
    (y',m') = rolloverMonths (y,fromIntegral m + n)

-- | Add months, with days past the last day of the month clipped to the last day.
-- For instance, 2005-01-30 + 1 month = 2005-02-28.
addJulianMonthsClip :: Integer -> Day -> Day
addJulianMonthsClip n day = fromJulian y m d where
    (y,m,d) = addJulianMonths n day

-- | Add months, with days past the last day of the month rolling over to the next month.
-- For instance, 2005-01-30 + 1 month = 2005-03-02.
addJulianMonthsRollOver :: Integer -> Day -> Day
addJulianMonthsRollOver n day = addDays (fromIntegral d - 1) (fromJulian y m 1) where
    (y,m,d) = addJulianMonths n day

-- | Add years, matching month and day, with Feb 29th clipped to Feb 28th if necessary.
-- For instance, 2004-02-29 + 2 years = 2006-02-28.
addJulianYearsClip :: Integer -> Day -> Day
addJulianYearsClip n = addJulianMonthsClip (n * 12)

-- | Add years, matching month and day, with Feb 29th rolled over to Mar 1st if necessary.
-- For instance, 2004-02-29 + 2 years = 2006-03-01.
addJulianYearsRollOver :: Integer -> Day -> Day
addJulianYearsRollOver n = addJulianMonthsRollOver (n * 12)

-- | Add months (clipped to last day), then add days
addJulianDurationClip :: CalendarDiffDays -> Day -> Day
addJulianDurationClip (CalendarDiffDays m d) day = addDays d $ addJulianMonthsClip m day

-- | Add months (rolling over to next month), then add days
addJulianDurationRollOver :: CalendarDiffDays -> Day -> Day
addJulianDurationRollOver (CalendarDiffDays m d) day = addDays d $ addJulianMonthsRollOver m day

-- | Calendrical difference, with as many whole months as possible
diffJulianDurationClip :: Day -> Day -> CalendarDiffDays
diffJulianDurationClip day2 day1 = let
    (y1,m1,d1) = toJulian day1
    (y2,m2,d2) = toJulian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    ymAllowed =
        if day2 >= day1 then
        if d2 >= d1 then ymdiff else ymdiff - 1
        else if d2 <= d1 then ymdiff else ymdiff + 1
    dayAllowed = addJulianDurationClip (CalendarDiffDays ymAllowed 0) day1
    in CalendarDiffDays ymAllowed $ diffDays day2 dayAllowed

-- | Calendrical difference, with as many whole months as possible.
-- Same as 'diffJulianDurationClip' for positive durations.
diffJulianDurationRollOver :: Day -> Day -> CalendarDiffDays
diffJulianDurationRollOver day2 day1 = let
    (y1,m1,d1) = toJulian day1
    (y2,m2,d2) = toJulian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    ymAllowed =
        if day2 >= day1 then
        if d2 >= d1 then ymdiff else ymdiff - 1
        else if d2 <= d1 then ymdiff else ymdiff + 1
    dayAllowed = addJulianDurationRollOver (CalendarDiffDays ymAllowed 0) day1
    in CalendarDiffDays ymAllowed $ diffDays day2 dayAllowed
