primes :: [Integer]
primes = 2 : (filter prime $ [3,5..])

prime :: Integer -> Bool
prime n = all (/=0) $ map (n `mod`) $ takeWhile (<= maxN n) primes
  where maxN n = (toInteger . truncate . sqrt . fromIntegral) n


main = do
  return (primes !! 10000)