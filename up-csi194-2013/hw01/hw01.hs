--------------------------------------------
-- homeWok01 : Validating Credit Card Numbers
---------------------------------------------

{- Validation Algorithm

   a. Double the value of every second digit beginning from the right.
      That is, the last digit is unchanged; the second-to-last digit is doubled; 
      the third-to-last digit is unchanged; and so on. 
      For example: [1,3,8,6] becomes [2,3,16,6].

   b. Add the digits of the doubled values and the undoubled digits from the original number. 
      For example, [2,3,16,6] becomes 2+3+1+6+6 = 18.

   c. Calculate the remainder when the sum is divided by 10. 
      For the above example, the remainder would be 8.
      If the result equals 0, then the number is valid
-}

{-Implementation:
   f1: Find the digits of a number
   f2: Double every other one
   f3: Sum digits
   f4: Validate
-}

-- Excercise 1
-- Convert positive integers to a list of digits; for 0 or negative input:return the empty list
toDigits :: Integer -> [Integer]
toDigits n
  | n<=0       = []
  | otherwise  = toDigits (n `div` 10) ++ [n `mod` 10]

-- Convert positive integers to a list of digits in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n<=0      = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- Excercise 2
-- Once you have the digits in the proper order ,double every other one  
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : y*2 : doubleEveryOther xs

-- Excercise 3
-- sumDigits
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = (div x 10) + (mod x 10) + sumDigits xs


-- Excercise 4
-- Validador
validate :: Integer -> Bool
validate n
   | mod (sumDigits (doubleEveryOther (toDigitsRev n))) 10 == 0 = True
   | otherwise = False

-------
-- EOF
-------  
