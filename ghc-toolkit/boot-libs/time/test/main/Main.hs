module Main where

import Test.Tasty
import Test.Calendar.AddDays
import Test.Calendar.Calendars
import Test.Calendar.ClipDates
import Test.Calendar.ConvertBack
import Test.Calendar.Duration
import Test.Calendar.Easter
import Test.Calendar.LongWeekYears
import Test.Calendar.MonthDay
import Test.Calendar.Valid
import Test.Calendar.Week
import Test.Clock.Conversion
import Test.Clock.Resolution
import Test.Clock.TAI
import Test.Format.Format
import Test.Format.ParseTime
import Test.Format.ISO8601
import Test.LocalTime.Time
import Test.LocalTime.TimeOfDay
import Test.LocalTime.CalendarDiffTime


tests :: TestTree
tests = testGroup "Time" [
    testGroup "Calendar" [
        addDaysTest,
        testCalendars,
        clipDates,
        convertBack,
        longWeekYears,
        testMonthDay,
        testEaster,
        testValid,
        testWeek,
        testDuration
        ],
    testGroup "Clock" [
        testClockConversion,
        testResolutions,
        testTAI
        ],
    testGroup "Format" [
        testFormat,
        testParseTime,
        testISO8601
        ],
    testGroup "LocalTime" [
        testTime,
        testTimeOfDay,
        testCalendarDiffTime
        ]
    ]

main :: IO ()
main = defaultMain tests
