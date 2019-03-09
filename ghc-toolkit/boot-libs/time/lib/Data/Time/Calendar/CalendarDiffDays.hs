module Data.Time.Calendar.CalendarDiffDays
    (
        -- * Calendar Duration
        module Data.Time.Calendar.CalendarDiffDays
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid
#endif
#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,11,0)
import Data.Semigroup hiding (option)
#endif
import Data.Typeable
import Data.Data

data CalendarDiffDays = CalendarDiffDays
    { cdMonths :: Integer
    , cdDays :: Integer
    } deriving (Eq,
    Data
#if __GLASGOW_HASKELL__ >= 802
    -- ^ @since 1.9.2
#endif
    ,Typeable
#if __GLASGOW_HASKELL__ >= 802
    -- ^ @since 1.9.2
#endif
    )

#if MIN_VERSION_base(4,9,0)
-- | Additive
instance Semigroup CalendarDiffDays where
    CalendarDiffDays m1 d1 <> CalendarDiffDays m2 d2 = CalendarDiffDays (m1 + m2) (d1 + d2)
#endif

-- | Additive
instance Monoid CalendarDiffDays where
    mempty = CalendarDiffDays 0 0
#if MIN_VERSION_base(4,9,0)
    mappend = (<>)
#else
    mappend (CalendarDiffDays m1 d1) (CalendarDiffDays m2 d2) = CalendarDiffDays (m1 + m2) (d1 + d2)
#endif

instance Show CalendarDiffDays where
    show (CalendarDiffDays m d) = "P" ++ show m ++ "M" ++ show d ++ "D"

calendarDay :: CalendarDiffDays
calendarDay = CalendarDiffDays 0 1

calendarWeek :: CalendarDiffDays
calendarWeek = CalendarDiffDays 0 7

calendarMonth :: CalendarDiffDays
calendarMonth = CalendarDiffDays 1 0

calendarYear :: CalendarDiffDays
calendarYear = CalendarDiffDays 12 0

-- | Scale by a factor. Note that @scaleCalendarDiffDays (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDiffDays :: Integer -> CalendarDiffDays -> CalendarDiffDays
scaleCalendarDiffDays k (CalendarDiffDays m d) = CalendarDiffDays (k * m) (k * d)
