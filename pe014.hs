import Data.Maybe (fromJust)
import Data.List (findIndex)

type Map = [Int]
initial = 1 : replicate 9999 0
l = length initial `div` 8

-- provede step podle zadani
step :: Int -> Int
step x | even x    = x `div` 2
       | otherwise = 3 * x + 1

stepCount :: Map -> Int -> Int
stepCount m n | n > length m    = 1 + stepCount m (step n)
              | m !! (n-1) /= 0 = (m !! (n-1))
              | otherwise       = 1 + stepCount m (step n)

updateMap :: Map -> Int -> Int -> Map
updateMap m n x = take (n-1) m ++ (x : drop n m)

countAll :: Map -> Map
countAll m = foldl (\x y -> snd (countOne x y)) m [1..l]

countOne :: Map -> Int -> (Int,Map)
countOne m n | n > length m    = let (x, newM) = countOne m (step n)
                                 in  (x+1, newM)
             | m !! (n-1) /= 0 = (m !! (n-1), m)
             | otherwise       = let (x, newM) = countOne m (step n)
                                 in  (x+1, updateMap newM n (x+1))

-- rozumne rychle jen do 10000 prvku
main = do
  let counted = countAll initial
  let maxSeq = maximum $ counted
  return $ fromJust (findIndex (==maxSeq) counted) + 1

------------------------ varianta 2 -----------------------------
-- nedeterministicky urci, ze ktereho cisla se stepem lze dostat na x
invStep :: Int -> Int
invStep x | (x - 1) `mod` 3 == 0 = (x - 1) `div` 3
          | otherwise            = x * 2

-- nefunguje, protoze rada neni linearni
stepSequence = 1 : iterate invStep 2

-- vypise steps od cisla n do 1cky
steps n = takeWhile (/=1) $ iterate step n

-- vrati vsechny variany cisel, ze kterych byl proveden step, kdyz vyslo x
mStep :: Int -> [Int]
mStep x | (x - 1) `mod` 3 == 0 = filter (/= 0) [x * 2, (x - 1) `div` 3]
        | otherwise            = [x * 2]

mStepWithUpdate :: (Map, [Int], Int) -> (Map, [Int], Int)
mStepWithUpdate (m,x,n) = let newX = filter (\a -> a <= l && m !! (a-1) == 0) $ x >>= mStep
                              newM = updateMapWithN m newX n
                          in  (newM, filter (unset newM) newX, n + 1)
  where unset m n = if n <= l then m !! (n - 1) /= 0
                              else True

updateMapWithN :: Map -> [Int] -> Int -> Map
updateMapWithN m []     _ = m
updateMapWithN m (n:ns) x = updateMapWithN (updateMap m n x) ns x

countMap :: Map -> Map
countMap m = countMapStep (m, [1], 2)

countMapStep :: (Map, [Int], Int) -> Map
countMapStep triple@(m, x, n) | x == []   = m
                              | otherwise = countMapStep $ mStepWithUpdate triple

-- rozumne rychle taky jen do 10000 prvku
-- main = do
--   print $ countMap initial

  

