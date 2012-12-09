import Data.List(group, sort, maximumBy)
import Data.Maybe(catMaybes)

p = 1000

pyth :: Int -> Int -> Maybe (Int, Int, Int)
pyth a b | rest == 0 = Just (a, b, c)
		 | otherwise = Nothing
	where (c, rest) = properFraction . sqrt . fromIntegral $ (a^2 + b^2)

perimeter :: (Int, Int, Int) -> Int
perimeter (a, b, c) = a + b + c

triples = [ pyth a b | b <- [1..p], a <- [1..b] ]

cmpLengths :: [a] -> [b] -> Ordering
cmpLengths x y = compare (length x) (length y)

main = print . maximumBy cmpLengths . group . sort . filter (<= p) . map perimeter . catMaybes $ triples