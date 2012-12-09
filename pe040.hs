import Data.Char(digitToInt)

decimals :: String
decimals = concat . map show $ [1..]

nthDigit :: Int -> String -> Int
nthDigit n x = digitToInt $ x !! (n-1)

main = do
  print . product . map (flip nthDigit decimals) $ [1,10,100,1000,10000,100000,1000000]
  