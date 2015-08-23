-- Copyright (c) 2015 Adrian "Boom" Nwk
-- Extensively revised by Bart Massey
-- Day-of-week computations

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Printf

data Months = Jan | Feb | Mar | Apr | May | Jun
            | Jul | Aug | Sep | Oct | Nov | Dec
              deriving (Show, Eq, Enum, Bounded, Ord)

data Days = Mon | Tue | Wed | Thu | Fri | Sat | Sun
            deriving (Show, Eq, Enum, Bounded)

data Date = Date {  dayOfWeek :: Days
                  , dayNumber :: Int
                  , month     :: Months
                  , year      :: Int }
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

monthDays :: Int -> Months -> Int
monthDays y m
    | m `elem` [Jan, Mar, May, Jul, Aug, Oct, Dec] = 31
    | m `elem` [Apr, Jun, Sep, Nov] = 30
    | isLeapYear y = 29
    | otherwise = 28

sums :: Integral a => [a] -> [a]
sums = scanl (+) 0

mdTable :: [Int]
mdTable = sums [monthDays 1 (toEnum m) | m <- [0..10]]

daysUpToMonth :: Int -> Months -> Int
daysUpToMonth y m
    | isLeapYear y && m > Feb = md + 1
    | otherwise = md
    where
      -- XXX This should be a binary search or an array.
      md = mdTable !! fromEnum m

instance Enum Date where
    fromEnum d = ((146097 * year d) `div` 400) +
                 daysUpToMonth (year d) (month d) + dayNumber d - 1
    toEnum n = Date { dayOfWeek = dow,
                      dayNumber = dn,
                      month = m,
                      year = y }
               where
                 -- http://stackoverflow.com/questions/11188621/
                 dow = toEnum $ (n + 6) `mod` 7
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
         days  = [Date Mon 2 Aug 2010..Date Sat 22 Aug 2015]
