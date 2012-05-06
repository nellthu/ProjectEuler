import Data.List (nub)

main = do
  return $ sum $ nub $ dividends 3 ++ dividends 5
    where dividends n = filter (\x -> x `mod` n == 0) [1..999]