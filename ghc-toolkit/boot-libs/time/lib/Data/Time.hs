{-|

= Quick Start

Use these types for time regardless of location (not caring about leap-seconds):

* 'UTCTime' for actual times
* 'NominalDiffTime' for differences between times, i.e. durations

Use these types for the ways people refer to time and time differences:

* 'Day' for something like June 27th 2017
* 'DayOfWeek' for something like Tuesday
* 'TimeOfDay' for something like 5pm
* 'LocalTime' for a 'Day' with a 'TimeOfDay'
* 'TimeZone' for a time zone offset (not actually the time zone itself) like -0700
* 'ZonedTime' for a 'LocalTime' with a 'TimeZone'
* 'CalendarDiffDays' for something like 6 years, 1 month and 5 days
* 'CalendarDiffTime' for something like 6 years, 1 month, 5 days, 3 hours, 7 minutes and 25.784 seconds

Use this for low-latency timing:

* 'Data.Time.Clock.System.SystemTime'

These are less commonly needed:

* 'Data.Time.Clock.TAI.AbsoluteTime' and 'DiffTime' if you do care about leap-seconds.
* 'Data.Time.Clock.TAI.LeapSecondMap' for tracking the leap-seconds
* 'UniversalTime' for time based on Earth rotation
-}
module Data.Time
(
    module Data.Time.Calendar,
    module Data.Time.Clock,
    module Data.Time.LocalTime,
    module Data.Time.Format
) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
