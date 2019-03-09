{-# OPTIONS -fno-warn-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Time.LocalTime.Internal.TimeZone
(
    -- * Time zones
    TimeZone(..),timeZoneOffsetString,timeZoneOffsetString',timeZoneOffsetString'',minutesToTimeZone,hoursToTimeZone,utc,

    -- getting the locale time zone
    getTimeZone,getCurrentTimeZone
) where

--import System.Time.Calendar.Format
import Data.Time.Calendar.Private
import Data.Time.Clock.System
import Data.Time.Clock.POSIX
import Data.Time.Clock.Internal.UTCTime

#if __GLASGOW_HASKELL__ >= 709
import Foreign
#else
import Foreign.Safe
#endif
import Foreign.C
import Control.DeepSeq
import Data.Typeable
import Data.Data

-- | A TimeZone is a whole number of minutes offset from UTC, together with a name and a \"just for summer\" flag.
data TimeZone = TimeZone {
    -- | The number of minutes offset from UTC. Positive means local time will be later in the day than UTC.
    timeZoneMinutes :: Int,
    -- | Is this time zone just persisting for the summer?
    timeZoneSummerOnly :: Bool,
    -- | The name of the zone, typically a three- or four-letter acronym.
    timeZoneName :: String
} deriving (Eq,Ord,Data, Typeable)

instance NFData TimeZone where
    rnf (TimeZone m so n) = rnf m `seq` rnf so `seq` rnf n `seq` ()

-- | Create a nameless non-summer timezone for this number of minutes.
minutesToTimeZone :: Int -> TimeZone
minutesToTimeZone m = TimeZone m False ""

-- | Create a nameless non-summer timezone for this number of hours.
hoursToTimeZone :: Int -> TimeZone
hoursToTimeZone i = minutesToTimeZone (60 * i)

showT :: Bool -> PadOption -> Int -> String
showT False opt t = showPaddedNum opt ((div t 60) * 100 + (mod t 60))
showT True opt t = let
    opt' = case opt of
        NoPad -> NoPad
        Pad i c -> Pad (max 0 $ i - 3) c
    in showPaddedNum opt' (div t 60) ++ ":" ++ show2 (mod t 60)

timeZoneOffsetString'' :: Bool -> PadOption -> TimeZone -> String
timeZoneOffsetString'' colon opt (TimeZone t _ _) | t < 0 = '-':(showT colon opt (negate t))
timeZoneOffsetString'' colon opt (TimeZone t _ _) = '+':(showT colon opt t)

-- | Text representing the offset of this timezone, such as \"-0800\" or \"+0400\" (like @%z@ in formatTime), with arbitrary padding.
timeZoneOffsetString' :: Maybe Char -> TimeZone -> String
timeZoneOffsetString' Nothing = timeZoneOffsetString'' False NoPad
timeZoneOffsetString' (Just c) = timeZoneOffsetString'' False $ Pad 4 c

-- | Text representing the offset of this timezone, such as \"-0800\" or \"+0400\" (like @%z@ in formatTime).
timeZoneOffsetString :: TimeZone -> String
timeZoneOffsetString = timeZoneOffsetString'' False (Pad 4 '0')

instance Show TimeZone where
    show zone@(TimeZone _ _ "") = timeZoneOffsetString zone
    show (TimeZone _ _ name) = name

-- | The UTC time zone.
utc :: TimeZone
utc = TimeZone 0 False "UTC"

{-# CFILES cbits/HsTime.c #-}
foreign import ccall unsafe "HsTime.h get_current_timezone_seconds" get_current_timezone_seconds :: CTime -> Ptr CInt -> Ptr CString -> IO CLong

getTimeZoneCTime :: CTime -> IO TimeZone
getTimeZoneCTime ctime = with 0 (\pdst -> with nullPtr (\pcname -> do
    secs <- get_current_timezone_seconds ctime pdst pcname
    case secs of
        0x80000000 -> fail "localtime_r failed"
        _ -> do
            dst <- peek pdst
            cname <- peek pcname
            name <- peekCString cname
            return (TimeZone (div (fromIntegral secs) 60) (dst == 1) name)
    ))

toCTime :: Int64 -> IO CTime
toCTime t = let
    tt = fromIntegral t
    t' = fromIntegral tt
    -- there's no instance Bounded CTime, so this is the easiest way to check for overflow
    in if t' == t then return $ CTime tt else fail "Data.Time.LocalTime.Internal.TimeZone.toCTime: Overflow"

-- | Get the local time-zone for a given time (varying as per summertime adjustments).
getTimeZoneSystem :: SystemTime -> IO TimeZone
getTimeZoneSystem t = do
    ctime <- toCTime $ systemSeconds t
    getTimeZoneCTime ctime

-- | Get the local time-zone for a given time (varying as per summertime adjustments).
getTimeZone :: UTCTime -> IO TimeZone
getTimeZone t = do
    ctime <- toCTime $ floor $ utcTimeToPOSIXSeconds t
    getTimeZoneCTime ctime

-- | Get the current time-zone.
getCurrentTimeZone :: IO TimeZone
getCurrentTimeZone = getSystemTime >>= getTimeZoneSystem
