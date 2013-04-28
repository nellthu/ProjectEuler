import Data.List (permutations)
import Data.Char (digitToInt, intToDigit)

pandigital :: Int -> Bool
pandigital x = l < 10 && all (`elem` w) [1..l]
    where w = map digitToInt $ show x
          l = length w

pandigitals :: [Int]
pandigitals = map (read . (map intToDigit)) . permutations $ [1..9]


multsRight :: Int -> Int -> Bool
multsRight x y = x * y == y

divide :: Int -> (Int, Int) -> (Int, Int, Int)
divide x (l1, l2) = let (a, as) = splitAt l1 (show x)
						(b, c)  = splitAt l2 as
					in  (read a, read b, read c)


main = print . take 10 $ pandigitals