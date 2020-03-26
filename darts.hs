darts :: (Real a) => a -> a -> Int
darts x y
   | rad > 10 = 0
   | rad > 5 = 1
   | rad > 1 = 5
   | otherwise = 10
   where rad = sqrt (realToFrac (x * x + y * y))
