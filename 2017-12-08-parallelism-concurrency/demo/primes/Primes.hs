module Primes (isPrime) where

import Numeric.Natural

iSqrt :: Natural -> Natural
iSqrt n = go 0 n
  where
    go lo hi
      | hi == 1 = 1
      | mid^2 <= n && n < (mid+1)^2 = mid
      | n `inRange` (lo^2, mid^2) = go lo mid
      | n `inRange` (mid^2, hi^2) = go mid hi
      | otherwise = error "Binary search left bounds in iSqrt!"
      where
        mid = lo + (hi - lo) `quot` 2

inRange :: Ord a => a -> (a, a) -> Bool
inRange n (lo, hi) = lo <= n && n <= hi

-- Divisors smaller than the square root of a number
smallDivisors :: Natural -> [Natural]
smallDivisors n = filter divides_n [2..iSqrt n]
  where
    divides_n x = n `rem` x == 0

isPrime :: Natural -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = null (smallDivisors n)
