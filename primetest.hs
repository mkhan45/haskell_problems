import System.Environment

isPrime :: (Integral n) => n -> Bool
isPrime n
   | n == 2 || n == 3 || n == 5 = True
   | n `mod` 2 == 0 || n `mod` 3 == 0 || n `mod` 5 == 0 = False
   | otherwise = all (==False) [n `mod` i == 0 || n `mod` (i + 2) == 0 | i <- [5,11..(round (sqrt (fromIntegral n))) - 1]]

main = do
   args <- getArgs
   if length args == 0 
      then putStrLn ("Invalid number of args, expected 1 got " ++ (show $ length args))
      else do
         let num = read $ head args :: Integer
         putStrLn $ show $ isPrime num
