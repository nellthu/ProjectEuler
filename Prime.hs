module Prime (primes, prime, primeFactors, factors) where

primes :: [Int]
primes = 2 : (filter prime $ [3,5..])

prime :: Int -> Bool
prime 1 = False
prime n | n < 0     = False
        | otherwise = all not $ map (n `divisibleBy`) $ takeWhile (<= truncSqrt n) primes

truncSqrt :: (Integral a) => a -> a
truncSqrt n = fromIntegral . toInteger . truncate . sqrt . fromIntegral $ n

primeFactors :: Int -> [Int]
primeFactors n = factor n primes
  where factor n (p:ps) | p * p > n         = [n]
                        | n `divisibleBy` p = p : factor (n `div` p) (p:ps)
                        | otherwise         = factor n ps                  
        

findFactor :: Int -> Int
findFactor n = head . filter (n `divisibleBy`) . takeWhile (<= truncSqrt n) $ primes

factors :: Int -> [Int]
factors n = filter (n `divisibleBy`) [1..(n `div` 2)] ++ [n]

divisibleBy :: Int -> Int -> Bool
x `divisibleBy` y = x `mod` y == 0

