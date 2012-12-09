import Prime (primes, prime)

rotate :: Int -> Int -> Int
rotate x n = read $ b ++ a
	where (a, b) = splitAt n (show x)

rotations :: Int -> [Int]
rotations x = map (rotate x) [1..length (show x)]

circular :: Int -> Bool
circular x = all prime $ rotations x

circularPrimes :: Int -> [Int]
circularPrimes n = filter circular $ takeWhile (<n) primes

main = print . length $ circularPrimes 1000000