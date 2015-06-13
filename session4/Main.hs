

module Main where

import qualified Network.HTTP as HTTP
import System.Environment (getArgs)


url :: String -> String
url sym = "http://ichart.yahoo.com/table.csv?s=" ++ sym

get :: String -> IO String
get url = HTTP.simpleHTTP (HTTP.getRequest url) >>= HTTP.getResponseBody

output :: [String] -> IO ()
output [sym] = do 
  csv <- get (url sym)
  case csv of
       '<':_ -> putStrLn $ "Sorry, symbol " ++ sym ++ " not found"
       otherwise -> putStrLn csv
output _ = putStrLn "usage: ./Main <symbol>"

main :: IO ()
main = getArgs >>= output

