import Data.List (sort, delete)

digit :: [Int] -> Int -> [Int]
digit range 0    = range
digit []    _    = []
digit range rest = let fact = product [1..(length range - 1)]
                       x = rest `div` fact
                       d = (sort range) !! x
                   in  d : digit (delete d range) (rest `mod` fact)

main = do
  -- print $ sort $ Data.List.permutations [0..9] !! 999999   -- too slow
  print $ digit [0..9] 999999