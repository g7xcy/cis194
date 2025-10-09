{-# OPTIONS_GHC -Wall #-}

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | otherwise = mod x 10 : toDigitsRev (div x 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse
    where
        doubleEveryOtherRev [] = []
        doubleEveryOtherRev [x] = [x]
        doubleEveryOtherRev (x : y : xs) = x : 2 * y : doubleEveryOtherRev xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . (>>= toDigits)

validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

