addMax :: Int -> Int -> Int -> Int
addMax n x y = n + max x y

maxPath :: [[Int]] -> [Int]
maxPath (line:[])   = line
maxPath (line:rest) = let result = (maxPath rest)
                      in  zipWith3 addMax line (init result) (tail result)

main = do
  content <- readFile "data/067triangle.txt"
  let triangle = (map (map (read :: String -> Int)) . map words . lines $ content)
  print $ maxPath triangle