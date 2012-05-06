triangles :: [Int]
triangles = scanl (+) 1 [2..]

truncSqrt :: (Integral a) => a -> a
truncSqrt n = fromIntegral . toInteger . truncate . sqrt . fromIntegral $ n

divisibleBy :: (Integral a) => a -> a -> Bool
x `divisibleBy` y = x `mod` y == 0

divisorCount :: (Integral a) => a -> Int
divisorCount n = 2 * (1 + length (filter (n `divisibleBy`) [2..(truncSqrt n)]))

-- printPairs :: Int -> Int -> IO ()
-- printPairs n m = do
--   let x = head . dropWhile ((<=m) . snd) . zip (list n) . map divisorCount $ list n
--   putStrLn $ show x
--   if snd x < 500 then printPairs (fst x) (snd x)
--                  else return ()
--     where list n = dropWhile (<=n) . filter even $ triangles

main = do
  -- printPairs 0 0
  print . head . dropWhile ((<500) . snd) . zip list . map divisorCount $ list
    where list = filter even triangles

