import Prime (factors)
import Data.List (delete)

d :: Int -> Int
d x = sum (init . factors $ x)

amicable :: Int -> Bool
amicable x = d (d x) == x && d x /= x

amicables :: Int -> [Int]
amicables n = filter amicable [1..(n - 1)]

main = print . sum . amicables $ 10000

