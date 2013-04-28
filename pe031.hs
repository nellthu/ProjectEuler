-- 1p    1p-1 = 1
-- 2p    1p-1, 2p-1 = 2
-- 3p    1p-1, 2p-1 = 2
-- 4p    1p-1, 2p-2 = 3
-- 5p    1p-1, 2p-2, 5p-1 = 4
-- 6p    1p-1, 2p-3, 5p-1 = 5
-- 7p    1p-1, 2p-3, 5p-2 = 6
-- 8p    1p-1; 2p-4, 5p-2 = 7
-- 9p    1p-1; 2p-4, 5p-3 = 8
-- 10p   1p-1; 2p-5, 5p-4, 10p-1 = 11
-- 11p   1p-1; 2p-5, 5p-5, 10p-1 = 12
-- 12p   1p-1; 2p-6, 5p-6, 10p-2 = 15


pennies :: [Int]
pennies = [1,2,5,10,20,50,100,200]

pennies' = drop 2 pennies

-- count :: Int -> Integer
-- count n | n <= 0    = 0
--         | otherwise = 1 + (toInteger n) `div` 2 + sum (map (cc . (n-)) pennies') + if n `elem` pennies' then 1 else 0

count :: Int -> Integer
count n | n <= 0    = 0
        | otherwise = 1 + (toInteger n) `div` 2 + sum (map (cc . (n-)) pennies') + if n `elem` pennies' then 1 else 0

-- nektere kombinace se pocitaji dvakrat, napr. u 15: jednou se zavola cc (10) a zapocita se 10-kova mince, podruhe cc (5) a zapocita se 5-kova, ta sama kombinace

-- count :: Int -> Integer
-- count n | n < 0    = 0
--         | n == 0   = 1
--         | otherwise = 1 + (toInteger n) `div` 2 + sum (map (cc . (n-)) pennies')

cc :: Int -> Integer
cc n | n <= 0    = 0
     | otherwise = countChain !! (n-1)

countChain :: [Integer]
countChain = map count [1..]