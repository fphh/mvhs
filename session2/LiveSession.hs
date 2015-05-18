

module LiveSession where

aufg1 = (\f x -> f x) (\x -> x+1) 8

ones = 1:2:ones

ys = 1:map (\x->x+1) xs


fib = 1:1:zipWith (\a b -> a+b) fib   (tail fib)


--  DifferenceEngine


p :: Integer -> Integer
p x = x^3 + 1

xs :: [Integer]
xs = map p [0..4]

diffsRec :: [Integer] -> [Integer]
diffsRec [] = error "Empty list"
diffsRec [x] = []
diffsRec (x:y:ys) = (y-x) : diffsRec (y:ys)

diffs :: [Integer] -> [Integer]
diffs xs = zipWith (-) (tail xs) xs

test :: [Integer] -> Bool
test xs = diffs xs == diffsRec xs

allZero :: [Integer] -> Bool
allZero xs = all (==0) xs

allDiffs :: [Integer] -> [[Integer]]
allDiffs xs = takeWhile (\x -> not (allZero x)) (iterate diffs xs)

