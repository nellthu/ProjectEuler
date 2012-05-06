fibs = 1 : 1 : (zipWith (+) fibs $ tail fibs)

main = do
  print $ snd $ head $ dropWhile (\x -> length (show $ fst x) < 1000) $ zip fibs [1..]