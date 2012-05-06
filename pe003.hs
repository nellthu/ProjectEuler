import Data.Maybe

findFactor :: Integer -> Maybe Integer
findFactor x = if length list > 0 
                  then Just $ head list
                  else Nothing
  where maxFactor = truncate $ sqrt $ fromInteger x
        list = filter ((== 0) . (x `mod`)) [2..maxFactor]

factors x = case findFactor x of Just n -> n : factors (x `div` n)
                                 Nothing -> [x]