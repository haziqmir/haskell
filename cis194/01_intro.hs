-- Credit Card Validation
-- 1. Double the value of every second digit starting from the right.
-- 2. Add all the digits.
-- 3. If the number divides by 0, it is valid.


-- Exercise 1
-- Convert an integer into a list of integers

lastDigit :: Integral a => a -> a
lastDigit = flip mod 10 -- flips the arguments

dropLastDigit :: Integral a => a -> a
dropLastDigit = flip div 10

toDigitsRev :: Integral a => a -> [a]
toDigitsRev n
  | n < 1     = []
  | otherwise = lastDigit n : toDigitsRev (dropLastDigit n)

toDigits :: Integral a => a -> [a]
toDigits = reverse . toDigitsRev


-- Exercise 2
-- Double every other integer

doubleEveryOther :: Integral a => [a] -> [a]
doubleEveryOther = reverse . zipWith (*) (cycle[1,2]) . reverse


-- Exercise 3
-- Sum all digits in a list
sumDigits :: Integral a => [a] -> a
sumDigits n = foldl (\acc x -> if x > 9 then acc + (sumDigits $ toDigits x) else acc + x) 0 n


-- Exercise 4
-- Validate the credit card number

validate :: Integral a => a -> Bool
validate = (== 0) . (flip mod 10) . sumDigits . doubleEveryOther . toDigits
