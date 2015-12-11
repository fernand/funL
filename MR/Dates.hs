-- This module is heavily ripped/inspired from github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Data/Dates.hs
-- TODO: smart dates, ie currentdate, quarters from now till a year ago
module MR.Dates (
    DateSpan(..),
    Interval(..),
    splitSpan,
    spanContainsDate,
    spanLeftShift,
    mkdatespan,
) where

import MR.Types

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import System.Locale (defaultTimeLocale)
import Data.Time.Format
import Data.Data
import Data.Maybe

spanDaySubtract :: Int -> DateSpan -> DateSpan
spanDaySubtract _ s@(DateSpan Nothing Nothing) = s
spanDaySubtract n (DateSpan Nothing (Just e)) = DateSpan Nothing (Just (addDays (toInteger (-n)) e))
spanDaySubtract n (DateSpan (Just s) Nothing) = DateSpan (Just (addDays (toInteger (-n)) s)) Nothing
spanDaySubtract n (DateSpan (Just s) (Just e)) = DateSpan (Just (addDays (toInteger (-n)) s)) (Just (addDays (toInteger (-n)) e))

-- TODO: iffyness in months and quarters, change to use stuff from splitSpan
spanLeftShift :: Interval -> DateSpan -> DateSpan
spanLeftShift _ s@(DateSpan Nothing Nothing) = s
spanLeftShift AllSpan (DateSpan (Just s) _) = DateSpan Nothing (Just s)
spanLeftShift (Days n) d = spanDaySubtract n d
spanLeftShift (Weeks n) d = spanDaySubtract (n*7) d
spanLeftShift (Months n) d = spanDaySubtract (n*30) d
spanLeftShift (Quarters n) d = spanDaySubtract (n*3*30) d
spanLeftShift (Years n) d = spanDaySubtract (n*365) d

-- | Does the span include the given date ?
spanContainsDate :: DateSpan -> Day -> Bool
spanContainsDate (DateSpan Nothing Nothing)   _ = True
spanContainsDate (DateSpan Nothing (Just e))  d = d < e
spanContainsDate (DateSpan (Just b) Nothing)  d = d >= b
spanContainsDate (DateSpan (Just b) (Just e)) d = d >= b && d < e

-- | Split a DateSpan into one or more consecutive spans at the specified interval.
splitSpan :: Interval -> DateSpan -> [DateSpan]
splitSpan _ (DateSpan Nothing Nothing) = [DateSpan Nothing Nothing]
splitSpan AllSpan          s = [s]
splitSpan (Days n)     s = splitspan startofday     (applyN n nextday)     s
splitSpan (Weeks n)    s = splitspan startofweek    (applyN n nextweek)    s
splitSpan (Months n)   s = splitspan startofmonth   (applyN n nextmonth)   s
splitSpan (Quarters n) s = splitspan startofquarter (applyN n nextquarter) s
splitSpan (Years n)    s = splitspan startofyear    (applyN n nextyear)    s

-- Split the given span using the provided helper functions:
-- start is applied to the span's start date to get the first sub-span's start date
-- next is applied to a sub-span's start date to get the next sub-span's start date
splitspan :: (Day -> Day) -> (Day -> Day) -> DateSpan -> [DateSpan]
splitspan _ _ (DateSpan Nothing Nothing) = []
splitspan start next (DateSpan Nothing (Just e)) = splitspan start next (DateSpan (Just $ start e) (Just $ next $ start e))
splitspan start next (DateSpan (Just s) Nothing) = splitspan start next (DateSpan (Just $ start s) (Just $ next $ start s))
splitspan start next span@(DateSpan (Just s) (Just e))
    | s == e = [span]
    | otherwise = splitspan' start next span
    where
      splitspan' start next (DateSpan (Just s) (Just e))
          | s >= e = []
          | otherwise = DateSpan (Just subs) (Just sube) : splitspan' start next (DateSpan (Just sube) (Just e))
          where subs = start s
                sube = next subs
      splitspan' _ _ _ = error' "won't happen, avoids warnings"

prevday :: Day -> Day
prevday = addDays (-1)
nextday = addDays 1
startofday = id

thisweek = startofweek
prevweek = startofweek . addDays (-7)
nextweek = startofweek . addDays 7
startofweek day = fromMondayStartWeek y w 1
    where
      (y,_,_) = toGregorian day
      (w,_) = mondayStartWeek day

thismonth = startofmonth
prevmonth = startofmonth . addGregorianMonthsClip (-1)
nextmonth = startofmonth . addGregorianMonthsClip 1
startofmonth day = fromGregorian y m 1 where (y,m,_) = toGregorian day

thisquarter = startofquarter
prevquarter = startofquarter . addGregorianMonthsClip (-3)
nextquarter = startofquarter . addGregorianMonthsClip 3
startofquarter day = fromGregorian y (firstmonthofquarter m) 1
    where
      (y,m,_) = toGregorian day
      firstmonthofquarter m = ((m-1) `div` 3) * 3 + 1

thisyear = startofyear
prevyear = startofyear . addGregorianYearsClip (-1)
nextyear = startofyear . addGregorianYearsClip 1
startofyear day = fromGregorian y 1 1 where (y,_,_) = toGregorian day

-- | Parse a couple of date string formats to a time type.
parsedateM :: String -> Maybe Day
parsedateM s = firstJust [ 
     parseTime defaultTimeLocale "%Y/%m/%d" s,
     parseTime defaultTimeLocale "%Y-%m-%d" s 
     ]

mkdatespan :: String -> String -> DateSpan
mkdatespan b = DateSpan (parsedateM b) . parsedateM

firstJust ms = case dropWhile (==Nothing) ms of
    [] -> Nothing
    (md:_) -> md

applyN :: Int -> (a -> a) -> a -> a
applyN n f = (!! n) . iterate f

error' :: String -> a
error' = error . toSystemString

type SystemString = String
-- | Convert a unicode string to a system string, encoding with UTF-8 if
-- we are on a posix platform with GHC < 7.2.
toSystemString :: String -> SystemString
-- #if __GLASGOW_HASKELL__ < 702
-- toSystemString = case os of
--                      "unix" -> UTF8.encodeString
--                      "linux" -> UTF8.encodeString
--                      "darwin" -> UTF8.encodeString
--                      _ -> id
-- #else
toSystemString = id
-- #endif