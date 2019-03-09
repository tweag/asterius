{-# OPTIONS -fno-warn-orphans #-}

module Test.Arbitrary where

import Control.Monad
import Data.Ratio
import Data.Time
import Data.Time.Clock.POSIX
import Test.Tasty.QuickCheck hiding (reason)

instance Arbitrary DayOfWeek where
    arbitrary = fmap toEnum $ choose (1,7)

instance Arbitrary Day where
    arbitrary = liftM ModifiedJulianDay $ choose (-313698, 2973483) -- 1000-01-1 to 9999-12-31
    shrink day = let
        (y, m, d) = toGregorian day
        dayShrink =
            if d > 1
                then [fromGregorian y m (d - 1)]
                else []
        monthShrink =
            if m > 1
                then [fromGregorian y (m - 1) d]
                else []
        yearShrink =
            if y > 2000
                then [fromGregorian (y - 1) m d]
                else if y < 2000
                         then [fromGregorian (y + 1) m d]
                         else []
        in dayShrink ++ monthShrink ++ yearShrink

instance CoArbitrary Day where
    coarbitrary (ModifiedJulianDay d) = coarbitrary d

instance Arbitrary CalendarDiffDays where
    arbitrary = liftM2 CalendarDiffDays arbitrary arbitrary

instance Arbitrary DiffTime where
    arbitrary = oneof [intSecs, fracSecs] -- up to 1 leap second
      where
        intSecs = liftM secondsToDiffTime' $ choose (0, 86400)
        fracSecs = liftM picosecondsToDiffTime' $ choose (0, 86400 * 10 ^ (12 :: Int))
        secondsToDiffTime' :: Integer -> DiffTime
        secondsToDiffTime' = fromInteger
        picosecondsToDiffTime' :: Integer -> DiffTime
        picosecondsToDiffTime' x = fromRational (x % 10 ^ (12 :: Int))

instance CoArbitrary DiffTime where
    coarbitrary t = coarbitrary (fromEnum t)

instance Arbitrary NominalDiffTime where
    arbitrary = oneof [intSecs, fracSecs]
      where
        limit = 1000 * 86400
        picofactor = 10 ^ (12 :: Int)
        intSecs = liftM secondsToDiffTime' $ choose (negate limit, limit)
        fracSecs = liftM picosecondsToDiffTime' $ choose (negate limit * picofactor, limit * picofactor)
        secondsToDiffTime' :: Integer -> NominalDiffTime
        secondsToDiffTime' = fromInteger
        picosecondsToDiffTime' :: Integer -> NominalDiffTime
        picosecondsToDiffTime' x = fromRational (x % 10 ^ (12 :: Int))

instance CoArbitrary NominalDiffTime where
    coarbitrary t = coarbitrary (fromEnum t)

instance Arbitrary CalendarDiffTime where
    arbitrary = liftM2 CalendarDiffTime arbitrary arbitrary

instance Arbitrary TimeOfDay where
    arbitrary = liftM timeToTimeOfDay arbitrary

instance CoArbitrary TimeOfDay where
    coarbitrary t = coarbitrary (timeOfDayToTime t)

instance Arbitrary LocalTime where
    arbitrary = liftM2 LocalTime arbitrary arbitrary

instance CoArbitrary LocalTime where
    coarbitrary t = coarbitrary (floor (utcTimeToPOSIXSeconds (localTimeToUTC utc t)) :: Integer)

instance Arbitrary TimeZone where
    arbitrary = liftM minutesToTimeZone $ choose (-720, 720)

instance CoArbitrary TimeZone where
    coarbitrary tz = coarbitrary (timeZoneMinutes tz)

instance Arbitrary ZonedTime where
    arbitrary = liftM2 ZonedTime arbitrary arbitrary

instance CoArbitrary ZonedTime where
    coarbitrary t = coarbitrary (floor (utcTimeToPOSIXSeconds (zonedTimeToUTC t)) :: Integer)

instance Arbitrary UTCTime where
    arbitrary = liftM2 UTCTime arbitrary arbitrary

instance CoArbitrary UTCTime where
    coarbitrary t = coarbitrary (floor (utcTimeToPOSIXSeconds t) :: Integer)

instance Arbitrary UniversalTime where
    arbitrary = liftM (\n -> ModJulianDate $ n % k) $ choose (-313698 * k, 2973483 * k) -- 1000-01-1 to 9999-12-31
      where
        k = 86400

instance CoArbitrary UniversalTime where
    coarbitrary (ModJulianDate d) = coarbitrary d
