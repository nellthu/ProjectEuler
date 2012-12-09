import Prime (primeFactors)
import Data.List (nub, delete)

distinct :: (Eq a) => [a] -> Bool
distinct list = and [ x /= y | x <- list, y <- (delete x list) ]

firstCons :: Int -> [Int] -> [Int]
firstCons n list | all (==n) (map length facts) && distinct facts = take n list
                 | otherwise = firstCons n (tail list)
    where facts = map (nub . primeFactors) (take n list)

main = (print . firstCons 4) [1..]

