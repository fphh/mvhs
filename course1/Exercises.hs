

module Exercise1 where


fibonacci :: Integer -> Integer
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

gcdAlt :: Integer -> Integer -> Integer
gcdAlt a 0 = a
gcdAlt a b = gcdAlt b (a `mod` b)


takeAlt :: Integer -> [a] -> [a]
takeAlt 0 _ = []
takeAlt n (x:xs) = x : takeAlt (n-1) xs

iterateAlt :: (a -> a) -> a -> [a]
iterateAlt f a = a : iterateAlt f (f a)

filterAlt :: (a -> Bool) -> [a] -> [a]
filterAlt p [] = []
filterAlt p (x:xs) =
 if p x
    then x : filterAlt p xs
    else filterAlt p xs

sumAlt :: [Integer] -> Integer
sumAlt xs = foldl (\acc x -> acc+x) 0 xs


