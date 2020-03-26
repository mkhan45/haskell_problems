divisors :: (Integral n) => n -> [n]
divisors x = [i | i <- [1..x], x `rem` i == 0]

perfect :: (Integral n) => n -> Bool
perfect x = sum (init (divisors x)) == x
