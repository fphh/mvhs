

module Main where

import Network.HTTP
import System.Environment (getArgs)

import qualified Data.List as List
import qualified Data.Map as Map

import Control.Applicative ((<*>))



url :: String -> String
url sym = "http://ichart.yahoo.com/table.csv?s=" ++ sym

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody


parseCsv :: String -> [Double]
parseCsv table =
  let (l:ls) = lines table
  in map (read . reverse . takeWhile (/= ',') . reverse) ls 

toLogYield :: [Double] -> [Double]
toLogYield xs = zipWith f xs (tail xs)
  where f x0 x1 = log (x0/x1)

inSomeDays :: Double -> Double -> Double -> [(Double, Double)]
inSomeDays mean stdDev lastPrice =
  let numberOfDays = 3
      mstd = mean + stdDev
      fs = [(* exp mstd), (* exp (-mstd))]
      vs = iterate (fs <*>) [lastPrice] !! numberOfDays
      rvs = map (\x -> fromIntegral (round (x*100)) /100) vs
      m = List.foldl' f Map.empty rvs
      f acc k = Map.insertWith (+) k 1 acc
      xs = Map.toList m
      len = fromIntegral $ length xs
      s = sum (map snd xs)
  in map (fmap (/s)) xs

calculateMeanAndStdDev :: [Double] -> (Double, Double, Double)
calculateMeanAndStdDev table =
  let logYield = toLogYield table
      len = fromIntegral (length logYield)
      mean = sum logYield / len
      stdDev = sqrt ((sum $ map (\x -> (x - mean)^2) logYield) / len)
  in (sum logYield, mean, stdDev)


output :: [String] -> IO ()
output [sym] = do 
  csv <- get (url sym)
  case csv of
       '<':_ -> putStrLn $ "Sorry, symbol " ++ sym ++ " not found"
       table -> do
         let parsedTable = parseCsv table
             (sly, mean, stdDev) = calculateMeanAndStdDev parsedTable
             lastPrice = head parsedTable
             ifd = inSomeDays mean stdDev lastPrice
         putStrLn csv
         putStrLn $ "Total Yield:\t" ++ show sly
         putStrLn $ "Mean:\t\t" ++ show mean
         putStrLn $ "StdDev:\t\t" ++ show stdDev
         putStrLn $ "Last Price:\t" ++ show lastPrice
         putStrLn ""
         putStrLn $ concatMap (\(a, b) -> show a ++ "\t" ++ show b ++ "\n") ifd


output _ = putStrLn "usage: ./Main <symbol>"

main :: IO ()
main = getArgs >>= output

