-- Copyright (c) 2015 Adrian "Boom" Nwk
-- Extensively revised by Bart Massey
-- Day-of-week computations

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Printf

data Month = Jan | Feb | Mar | Apr | May | Jun
            | Jul | Aug | Sep | Oct | Nov | Dec
              deriving (Show, Eq, Enum, Bounded, Ord)

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
            deriving (Show, Eq, Enum, Bounded)

data Date = Date { dayNumber :: Int,
                   month     :: Month,
                   year      :: Int }
            deriving Eq

instance Show Date where
    show d = printf "%s %d/%s/%d" (show (dayOfWeek d))
             (dayNumber d) (show (month d)) (year d)

divides :: Integral a => a -> a -> Bool
divides d n | n < 0 = divides d (-n)
divides d n = n `mod` d == 0

isLeapYear :: Int -> Bool
isLeapYear y
    | 400 `divides` y  = True
    | 100 `divides` y  = False
    | 4 `divides` y  = True
    | otherwise = False

monthDays :: Int -> Month -> Int
monthDays y m
    | m `elem` [Jan, Mar, May, Jul, Aug, Oct, Dec] = 31
    | m `elem` [Apr, Jun, Sep, Nov] = 30
    | isLeapYear y = 29
    | otherwise = 28

sums :: Integral a => [a] -> [a]
sums = scanl (+) 0

mdTable :: [Int]
mdTable = sums [monthDays 1 (toEnum m) | m <- [0..10]]

daysUpToMonth :: Int -> Month -> Int
daysUpToMonth y m
    | isLeapYear y && m > Feb = md + 1
    | otherwise = md
    where
      -- XXX This should be a binary search or an array.
      md = mdTable !! fromEnum m

dayOfWeekAbsolute :: Int -> Day
dayOfWeekAbsolute n = toEnum $ (n + 719527 + 6) `mod` 7

dayOfWeek :: Date -> Day
dayOfWeek d = dayOfWeekAbsolute $ fromEnum d

instance Enum Date where

    -- XXX We arbitrarily pick "day 0" to be 1 January 1970 (day 719527 A.D.).
    -- Any valid A.D. Gregorian calendar date should work.
    fromEnum d = ((146097 * year d) `div` 400) +
                 daysUpToMonth (year d) (month d) + dayNumber d - 719527 - 1

    toEnum n0 = Date { dayNumber = dn,
                      month = m,
                      year = y }
               where
                 -- http://stackoverflow.com/questions/11188621/
                 n = n0 + 719527
                 qcents = n `div` 146097
                 qcentDays = n `mod` 146097
                 cents = min (qcentDays `div` 36524) 3
                 centDays = qcentDays - cents * 36524
                 quads = min (centDays `div` 1461) 24
                 quadDays = centDays - quads * 1461
                 anns = min (quadDays `div` 365) 3
                 annDays = quadDays - anns * 365
                 y = qcents * 400 + cents * 100 + quads * 4 + anns
                 -- XXX This should be a binary search or an array.
                 m = fromJust $
                     find (\m0 -> daysUpToMonth y m0 <= annDays)
                          [Dec, Nov .. Jan]
                 dn = annDays - daysUpToMonth y m + 1

main :: IO ()
main = putStr $ unlines $ map show $
       [x | x <- days, dayOfWeek x `elem` [Mon, Wed, Fri]]
       where
         days  = [Date 2 Aug 2010..Date 22 Aug 2015]
