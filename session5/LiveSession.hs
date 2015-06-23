

module Main where


import System.Environment (getArgs)
import Network.HTTP

url :: String -> String
url sym = "http://ichart.yahoo.com/table.csv?s=" ++ sym

get :: String -> IO String
{-
get sym = do
  x <- simpleHTTP (getRequest sym)
  getResponseBody x
-}
get sym = simpleHTTP (getRequest sym) >>= getResponseBody


-- map f (map g xs) == map (f . g) xs

parseCsv :: String -> [[Double]]
parseCsv str =
  let _:ls = lines str 
  in map (map read . tail . splitIf (/=',')) ls

splitIf :: (a -> Bool) -> [a] -> [[a]]
--splitIf _ [] = []
splitIf p xs =
  case span p xs of
       (as, _:bs) -> as : splitIf p bs
       (as, []) -> [as]

output :: [String] -> IO ()
output [sym] = do
  csv <- get (url sym)
  print csv
  print (parseCsv csv)
output _ = putStrLn "Fehler"

main :: IO ()
main = getArgs >>= output

