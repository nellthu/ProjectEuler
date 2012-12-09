import Prime (prime)
import Data.List (nub, find, permutations, sort)
import Data.Char (digitToInt, intToDigit)

pandigital :: Int -> Bool
pandigital x = l < 10 && all (`elem` w) [1..l]
    where w = map digitToInt $ show x
          l = length w

pandigitals :: Int -> [Int]
pandigitals n = reverse . sort . map (read . (map intToDigit)) . permutations $ [1..n]

main = print . head . filter prime . concatMap pandigitals $ [9,8..4]

