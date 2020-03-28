import Data.List

solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs

splitOn :: (Eq a) => a -> [a] -> ([a], [a])
splitOn v ls = case elemIndex v ls of
                 Just i -> splitAt i ls
                 Nothing -> (ls, [])

infixToPostfix :: String -> [String] -> String
infixToPostfix "" _ = ""
infixToPostfix ls stack
   | token == "(" = infixToPostfix xs (stack ++ [token])
   | token `elem` ["+", "-" "*" "/"] = 
      where (token, xs) = splitOn ' ' ls
