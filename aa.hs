doublen n = n + n

lucky :: (Integral n) => n -> String
lucky 7 = "LUCKY"
lucky x = "unlucky :("

fib 0 = 1
fib 1 = 1
fib x = fib(x - 1) + fib(x - 2)

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

factorial :: (Integral a) => a -> a
factorial n = product [1..n]

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ "." ++ [l] ++ "."
