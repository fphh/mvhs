

module Main where

import Network.HTTP
import System.Environment (getArgs)

import qualified Data.List as List
import qualified Data.Map as Map

import Data.Ratio ((%))

import Control.Applicative ((<*>))



newtype Mean = Mean Double deriving (Show)

newtype StdDev = StdDev Double deriving (Show)

newtype LastPrice = LastPrice Double deriving (Show)

data LogYield = LogYield Int Double [Double] deriving (Show)


url :: String -> String
url sym = "http://ichart.yahoo.com/table.csv?s=" ++ sym

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody
-- get _ = readFile "GOOG.csv"

splitIf :: (a -> Bool) -> [a] -> [[a]]
splitIf p xs =
  case span p xs of
       (as, []) -> [as]
       (as, _:bs) -> as : splitIf p bs

parseCsv :: String -> [[Double]]
parseCsv table =
  let (l:ls) = lines table
  in map (map read . splitIf (/=',')) ls

toLogYield :: [Double] -> LogYield
toLogYield xs = LogYield (length ly) (sum ly) ly
  where ly = zipWith f xs (tail xs)
        f x0 x1 = log (x0/x1)

mean :: LogYield -> Mean
mean (LogYield len sumLogYield logYield) =
  Mean $ sumLogYield / fromIntegral len

stdDev :: Mean -> LogYield -> StdDev
stdDev (Mean mean) (LogYield len sumLogYield logYield) =
  StdDev $ sqrt $ (sum (map (\x -> (x-mean)^2) logYield) / fromIntegral len)


toFixed :: Double -> Double
toFixed x = fromInteger (round (x*prec)) / prec
  where prec = 1000

sort :: [Double] -> [(Double, Double)]
sort xs = map f $ List.group $ List.sort xs
  where len = fromIntegral (length xs)
        f ys@(y:_) = (y, fromIntegral (length ys) / len)

predict :: Mean -> StdDev -> LastPrice -> [[Double]]
predict (Mean mean) (StdDev stdDev) (LastPrice start) =
  let fs = [(* exp (mean+stdDev)), (* exp (mean - stdDev))]
  in iterate (fs <*>) [start]

predictSorted :: Mean -> StdDev -> LastPrice -> [[(Double, Double)]]
predictSorted mean stdDev lastPrice =
  map (sort . map toFixed) $ predict mean stdDev lastPrice

output :: [String] -> IO ()
output [sym] = do 
  csv <- get (url sym)
  case csv of
       '<':_ -> putStrLn $ "Sorry, symbol " ++ sym ++ " not found"
       table -> do
         let lastCol = map last $ parseCsv table
             logYield = toLogYield lastCol
             m = mean logYield
             sd = stdDev m logYield
             lastPrice = LastPrice (head lastCol)
             days = 7
             ps = take days $ predictSorted m sd lastPrice
             f idx ps = "Day " ++ show idx ++ "\n" ++ show ps
         putStrLn csv
         print m
         print sd
         putStrLn "\nPrecition\n"
         putStrLn (List.intercalate "\n\n" $ zipWith f [0..] ps)


output _ = putStrLn "usage: ./Main <symbol>"

main :: IO ()
main = getArgs >>= output
