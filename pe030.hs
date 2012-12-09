import Data.Char(digitToInt)


limit = 354294 -- 6 * 9^5

digits :: Int -> [Int]
digits n = map digitToInt $ show n

predicate :: Int -> Bool
predicate n = (sum . map (^5) . digits) n == n

main = do
  print . sum . filter predicate $ [1..limit]