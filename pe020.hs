import Data.Char (digitToInt)

digits = map digitToInt $ show $ product [1..100] :: [Int]

main = do
  return $ sum digits