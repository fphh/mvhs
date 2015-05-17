

module DifferenceEngine where

p :: Integer -> Integer
p x = x^3 - x^2

xs :: [Integer]
xs = map p [0 .. 4]

diffs :: [Integer] -> [Integer]
diffs [] = error "no input"
diffs [x] = []
diffs (x:y:ys) = (y-x) : diffs (y:ys)

diffs2 :: [Integer] -> [Integer]
diffs2 xs = zipWith (\a b -> a-b) (tail xs) xs

allZero :: [Integer] -> Bool
allZero xs = all (\x -> x==0) xs

allDiffs :: [Integer] -> [[Integer]]
allDiffs xs = takeWhile (\as -> not (allZero as)) (iterate diffs2 xs)

next :: [Integer] -> Integer
next xs = sum (map last (allDiffs xs))

test :: Bool
test = next xs == p 5

