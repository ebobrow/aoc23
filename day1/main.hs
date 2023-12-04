import Data.Char
import Data.Functor
import Data.List
import System.IO

-- Takes the first and last digit in a string and combines them into a two-digit number
-- "Hey Elliot why did you make this function absolutely illegible?" - Haters
calibrationValue = (((+) <$> (10 *) . head) <*> last) . map digitToInt . filter isDigit

part1 = readFile "/home/elliotbobrow/Downloads/day1" <&> sum . map calibrationValue . lines

digits xs
    | null xs = []
    | "one" `isPrefixOf` xs = 1 : digits (tail xs)
    | "two" `isPrefixOf` xs = 2 : digits (tail xs)
    | "three" `isPrefixOf` xs = 3 : digits (tail xs)
    | "four" `isPrefixOf` xs = 4 : digits (tail xs)
    | "five" `isPrefixOf` xs = 5 : digits (tail xs)
    | "six" `isPrefixOf` xs = 6 : digits (tail xs)
    | "seven" `isPrefixOf` xs = 7 : digits (tail xs)
    | "eight" `isPrefixOf` xs = 8 : digits (tail xs)
    | "nine" `isPrefixOf` xs = 9 : digits (tail xs)
    | isDigit $ head xs = digitToInt (head xs) : digits (tail xs)
    | otherwise = digits (tail xs)

calibrationValue' = (((+) <$> (10 *) . head) <*> last) . digits

part2 = readFile "/home/elliotbobrow/Downloads/day1" <&> sum . map calibrationValue' . lines
