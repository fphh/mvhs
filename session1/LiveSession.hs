
fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)



-- euclid :: Integer -> Integer -> Integer
euclid a 0 = a
euclid a b = euclid b (a `mod` b)

toList :: Integer -> [Integer]
toList 0 = []
-- toList n = [1] ++ toList (n-1)
toList n = 1 : toList (n-1)

fromList :: [Integer] -> Integer
fromList [] = 0
fromList (x:xs) = fromList xs + 1


test :: Integer -> Bool
test n = fromList (toList n) == n

-- mapF :: (Integer -> [Integer]) -> [Integer] -> [[Integer]]

mapF f [] = []
mapF f (x:xs) = f x : mapF f xs


testMapF xs = mapF fromList (mapF toList xs) == xs

question xs = mapF (mapF toList) (mapF toList xs)
