main = do
  print $ head [ (x, y, z) | x <- [1..333], y <- [x..666], let z = 1000-x-y, z > y, x^2+y^2==z^2 ]
