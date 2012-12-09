import Numeric(showIntAtBase)
import Data.Char(intToDigit)

palindrome :: Int -> Int -> Bool
palindrome base x = s == reverse s
	where s = (showIntAtBase base intToDigit x) ""

main = print . sum . filter (palindrome 2) . filter (palindrome 10) $ [1..1000000]