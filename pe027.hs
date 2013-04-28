import Prime
import Data.List(maximumBy)

-- bring a bucket for vomit

formula :: Int -> Int -> Int -> Int
formula a b n = n ^ 2 + a * n + b

score :: (Int -> Int) -> Int
score f = length . takeWhile prime . map f $ [0..]

result :: [(Int -> Int, Int)]
result = let r = [ formula a b | a <- [(-999)..999], b <- [(-999)..999] ]
         in  filter (\a -> snd a > 50) . zip r $ (map score r)

main = print . map (\x -> (fst x 0, fst x 1)) $ result
  