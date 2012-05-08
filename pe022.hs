import Data.List (sort)
import Data.Maybe (fromJust)

characterValue :: Char -> Int
characterValue c = fromJust $ lookup c $ zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" [1..]

stringValue :: String -> Int
stringValue s = sum $ map characterValue s

main = do
  result <- readFile $ "data/022names.txt"
  let names = sort (read ("[" ++ result ++ "]") :: [String])
  print $ sum $ zipWith (*) (map stringValue names) [1..]