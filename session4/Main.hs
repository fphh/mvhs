

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

output :: [String] -> IO ()
output [sym] = do 
  csv <- get (url sym)
  case csv of
       '<':_ -> putStrLn $ "Sorry, symbol " ++ sym ++ " not found"
       otherwise -> putStrLn csv
output _ = putStrLn "usage: ./Main <symbol>"

main :: IO ()
main = getArgs >>= output

