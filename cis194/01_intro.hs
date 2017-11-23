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


-- Exercise 5
-- Return the list of moves to solve a Towers of Hanoi configuration

type Peg = String
type Move = (Peg, Peg)

-- In order to move n discs from a to c:
-- Move n-1 discs from a to b using c
-- Move the nth disc from a to c
-- Move n-1 discs from b to c using a

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 0    = []
-- | n == 1    = [(a,c)]
  | otherwise = hanoi (n-1) a c b ++ ((a,c) : hanoi (n-1) b a c)


-- Optional: Exercise 6
-- Use 4 pegs which lets us use one more temporary peg

hanoi' :: Integer -> Peg -> Peg -> [Peg] -> [Move]
hanoi' 1 src tgt _ = [(src, tgt)]
hanoi' n src tgt (peg1:peg2:_) = hanoi' k src peg1 [peg2, tgt] ++ hanoi' (n-k) src tgt [peg2] ++ hanoi' k peg1 tgt [peg2, src]
  where k = quot n 2
hanoi' n src tgt (peg:_) = hanoi n src tgt peg


hanoi'' :: Integer -> [Peg] -> [Move]
hanoi'' 0 _ = []
hanoi'' 1 (peg1 : peg2 : _) = [(peg1, peg2)]
hanoi'' n (peg1 : peg2 : peg3 : others) =
  hanoi'' k (peg1 : peg3 : peg2 : others) ++
  hanoi'' (n-k) (peg1 : peg2 : others) ++
  hanoi'' k (peg3 : peg2 : peg1 : others)
  where k
         | n == 3    = n-1
         | otherwise = quot n 2
