-- elegant solution:
fibs = 1:1: zipWith (+) fibs (tail fibs)

-- my solution:

fib' :: (Int,Int) -> (Int,Int)
fib' (x,y) = (x+y,x)

fib_seq :: [Int]
fib_seq = map fst $ iterate fib' (2,1)

main = do
  return $ sum $ filter even $ takeWhile (<= 4000000) fib_seq