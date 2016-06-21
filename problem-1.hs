{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}


module Problem1 where
import Data.List (foldl)

sumOfMultiplesOf3And5 :: Int -> Int
sumOfMultiplesOf3And5 upperLimit =
  foldl (+) 0 candidates
  where
    numbersUnderLimit = [0..(upperLimit-1)]
    candidates = filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) numbersUnderLimit


test :: Bool
test =
  sumOfMultiplesOf3And5 10 == 23
