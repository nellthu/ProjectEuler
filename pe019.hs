type Year = Int
type Day = Int

daysInMonth :: Year -> Int -> Int
daysInMonth y m | m `elem` [4,6,9,11]        = 30
                | m `elem` [1,3,5,7,8,10,12] = 31
                | y `mod` 400 == 0           = 29
                | y `mod` 100 == 0           = 28
                | y `mod` 4 == 0             = 29
                | otherwise                  = 28

isSunday :: Day -> Bool
isSunday d = d `mod` 7 == 0

daysInYear :: Year -> [Int]
daysInYear y = map (daysInMonth y) [1..12]

start :: Int
start = 1 + sum (daysInYear 1900)

fun :: (Day -> Bool) -> Int -> Int -> [Int] -> Int
fun _     _     c     []                           = c
fun check begin count (month:months) | check begin = fun check (begin+month) (count+1) months
                                     | otherwise   = fun check (begin+month) count     months


main = do
  print $ fun isSunday start 0 $ concatMap daysInYear [1901..2000]