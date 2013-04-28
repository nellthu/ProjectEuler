import Control.Monad (liftM2)

roll :: [Int] -> [Int] -> Int -> [Int]
roll add list 1 = list
roll add list n = roll add (liftM2 (+) list add) (n-1) 

peter = roll [1..4] [1..4] 9
colin = roll [1..6] [1..6] 6
        
games = [ (x,y) | x <- peter, y <- colin ]

main = do
  print . (`div` length games) . length . filter (\g -> fst g > snd g) $ games