-- https://wiki.haskell.org/99_questions/1_to_10

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

secondLast :: [a] -> a
secondLast [a, b] = a
secondLast (_:xs) = secondLast xs

elementat :: (Integral n) => [a] -> n -> a
elementat ls 1 = head ls
elementat (_:xs) n = elementat xs (n - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)

myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome ls = ls == (reverse ls)

compress :: (Eq a) => [a] -> [a]
compress [a] = [a]
compress (x:ys@(y:_)) = 
   if x == y 
      then compress ys
      else x : compress ys

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = 
   let packed = pack xs in
       if x `elem` (head packed)
          then (x : (head packed)) : (tail (packed))
          else [x] : packed

run_len_encode :: (Eq a) => [a] -> [(Int, a)]
run_len_encode [] = []
run_len_encode [x] = [(1, x)]
run_len_encode (x:xs) =
   let next = run_len_encode xs 
       (next_cnt, next_x) = head next 
    in
      if x == next_x 
         then (next_cnt + 1, next_x) : (tail next)
         else (1, x) : next

run_len_decode :: (Eq a) => [(Int, a)] -> [a]
run_len_decode [] = []
run_len_decode ((cnt, x):xs) =
   (replicate cnt x) ++ (run_len_decode xs)

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x, x] ++ (dupli xs)

repli :: [b] -> Int -> [b]
repli [] _ = []
repli (x:xs) cnt = (replicate cnt x) ++ (repli xs cnt)

dropEvery :: (Eq a) => [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery ls cnt = 
   if snd /= []
      then fst ++ (dropEvery (tail snd) cnt)
      else fst
   where (fst, snd) = splitAt (cnt - 1) ls

split :: [a] -> Int -> ([a], [a])
split [] _ = error "can't split an empty list"
split ls i = (take i ls, drop i ls)

slice :: [a] -> Int -> Int -> [a]
slice ls start end = take (end - start) (drop (start - 1) ls)

rotate :: [a] -> Int -> [a]
rotate ls n = snd ++ fst 
                  where (fst, snd) = splitAt n ls

removeAt :: Int -> [a] -> (a, [a])
removeAt i ls = ((ls !! i), fst ++ (tail snd))
                  where (fst, snd) = splitAt i ls

insertAt :: Int -> a -> [a] -> [a]
insertAt i x ls = fst ++ [x] ++ snd 
                     where (fst, snd) = splitAt i ls

range :: Integer -> Integer -> [Integer]
range start end
   | end - start == 0 = []
   | otherwise = start : range (start + 1) end

isPrime :: (Integral n) => n -> Bool
isPrime n
   | n == 2 || n == 3 || n == 5 = True
   | n `mod` 2 == 0 || n `mod` 3 == 0 || n `mod` 5 == 0 = False
   | otherwise = all (==False) [n `mod` i == 0 || n `mod` (i + 2) == 0 | i <- [5,11..(round (sqrt (fromIntegral n))) - 1]]

myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = gcd b (a `mod` b)

coprime :: Int -> Int -> Bool
coprime a b = (gcd a b) == 1

totient :: Int -> Int
totient x = sum [fromEnum (coprime x n) | n <- [0..x-1]]

pollard :: (Integral a) => a -> a
pollard n 
   | isPrime n = n
   | n `mod` 2 == 0 = 2
   | sqrtn - (fromIntegral (round sqrtn)) < 0.01 = round sqrtn
   | otherwise =
         let cycleFind x y d n =
                if d /= 1 then d
                else 
                   let g a b = (a * a + 1) `mod` b
                       newx = g x n
                       newy = g (g y n) n
                       newd = gcd (abs (newx - newy)) n
                   in cycleFind newx newy newd n
         in cycleFind 2 2 1 n
     where sqrtn = sqrt $ fromIntegral n

factor :: (Integral a) => a -> (a -> a) -> [a]
factor n factorfn
  | isPrime n = [n]
  | otherwise = 
     let fac = factorfn n
     in fac : factor (n `div` fac) factorfn

primesRange :: (Integral a) => a -> a -> [a]
primesRange start end = [x | x <- [start..end], isPrime x]

goldbach :: Integer -> (Integer, Integer)
goldbach n = let primes = primesRange 2 n
             in head [(x, y) | x <- primes, y <- primes, x + y == n]
