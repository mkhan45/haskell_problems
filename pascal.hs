pascalTri :: Int -> [[Int]]
pascalTri 1 = [[1]]
pascalTri n = pascalTri (n - 1) ++ [[i * (n - i) `div` (i + 1) + 1 | i <- [0..n-1]]]

printPascal :: [[Int]] -> IO ()
printPascal tri = mapM_ print tri
