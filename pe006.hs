difference :: Integer -> Integer
difference n = (sum [1..n]) ^ 2 - sum [ x ^ 2 | x <- [1..n] ]