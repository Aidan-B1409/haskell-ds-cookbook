import Text.CSV
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)

main :: IO ()
main = do
  let fileName = "app/ages.csv"
  input <- readFile fileName
  let csv = parseCSV fileName input
  either handleError doWork csv
  
handleError csv = putStrLn "not a CSV"
doWork csv = (print.findOldest.tail) (filter (\x -> length x == 2) csv)

-- Finds oldest person.
findOldest :: [Record] -> Record
findOldest [] = []
findOldest items = foldl1 (\a x -> if age x > age a then x else a) items

age [a,b] = toInt b

toInt :: String -> Int                               
toInt = read