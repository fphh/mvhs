
module LiveSession where


import Control.Applicative ((<*>), liftA2)

import qualified Data.Map as Map
import Data.Map (Map)

mapInto :: (a -> b) -> Maybe a -> Maybe b
mapInto _ Nothing = Nothing
mapInto f (Just x) = Just (f x)

sequential :: Maybe (a -> b) -> Maybe a -> Maybe b
sequential Nothing _ = Nothing
sequential (Just f) Nothing = Nothing
sequential (Just f) (Just x) = Just (f x)

env :: Map String Integer
env = Map.fromList [("a", 7), ("b", 10), ("c", 17)]

getInteger :: IO Integer
getInteger = fmap read getLine

sum2 = lA2 (\x y -> x+y) getInteger getInteger

lA2 f x y = fmap f x <*> y

-- type IO a = World -> (a, World)

unit :: a -> Maybe a
unit = Just

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind (Just x) f = f x
bind Nothing _ = Nothing

testIO :: IO Integer
testIO = do
  x <- getInteger
  y <- getInteger
--   putStrLn ("x+y=" ++ show (x+y))
