import Prime
import Data.List(tails)

limit = 1000000

sumPrimes :: [Int] -> Int
sumPrimes list = last . takeWhile (<limit) . filter prime . scanl1 (+) $ list

main = do
  print . maximum . map sumPrimes . take 10 . tails $ primes

