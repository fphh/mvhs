
module Sieve where




filterMultiples :: Integer -> [Integer] -> [Integer]
filterMultiples n xs =
  filter (\x -> mod x n /= 0) xs

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve (filterMultiples x xs)

primes :: [Integer]
primes = sieve [2..]