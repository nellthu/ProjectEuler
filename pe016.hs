import Data.Char (digitToInt)

main = do
  print . sum . map digitToInt . show $ 2 ^ 1000