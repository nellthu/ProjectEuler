isPalindrome :: String -> Bool
isPalindrome x | length x < 2 = True
               | head x == last x = isPalindrome $ tail $ init x
               | otherwise = False

main = do
  return $ maxTuple (0,(0,0)) [ (x * y, (x, y)) | x <- [1..999], y <- [1..999], isPalindrome $ show $ x * y ]
    where maxTuple m []                     = m
          maxTuple m (x:xs) | fst x > fst m = maxTuple x xs
                            | otherwise     = maxTuple m xs