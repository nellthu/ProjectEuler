smallest :: Integer -> Integer
smallest n = foldl addFactor 1 [2..n]
  where addFactor n x = n * x `div` (gcd n x)
  