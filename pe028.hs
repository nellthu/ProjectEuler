diagonal n = scanl (+) 1 $ iterate (+8) n

southeast = diagonal 2
southwest = diagonal 4
northwest = diagonal 6
northeast = diagonal 8

main = do
  print $ sum (take 501 northwest ++ take 501 northeast ++ take 501 southwest ++ take 501 southeast) - 3