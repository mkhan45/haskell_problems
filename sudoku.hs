import System.Environment
import Data.List
import Data.Char
import Debug.Trace

initBoard :: [[Int]]
initBoard = replicate 9 (replicate 9 0)

showBoard :: [[Int]] -> String
showBoard [] = ""
showBoard board = (show (head board)) ++ "\n" ++ (showBoard (tail board))

boardComplete :: [[Int]] -> Bool
boardComplete board = all (==[1..9]) (map sort (boardSets board))

setValid :: [Int] -> Bool
setValid ls = not $ hasDups ls []
               where 
                  hasDups :: [Int] -> [Int] -> Bool
                  hasDups [] _ = False
                  hasDups set@(x:xs) used = if x /= 0 && x `elem` used 
                                           then True
                                           else hasDups xs (x : used)

boardCols :: [[Int]] -> [[Int]]
boardCols board = transpose board

getSquare :: Int -> [[Int]] -> [Int]
getSquare i board = concat [take 3 (drop start_col row) | row <- rows]
                     where start_row = i `mod` 3 * 3
                           start_col = i `div` 3 * 3
                           rows = take 3 (drop start_row board)

boardSquares :: [[Int]] -> [[Int]]
boardSquares board = [getSquare square_i board | square_i <- [0..8]]

boardSets :: [[Int]] -> [[Int]]
boardSets board = board ++ (boardCols board) ++ (boardSquares board)

boardValid :: [[Int]] -> Bool
boardValid board = all setValid (boardSets board)

replaceIndex :: (Int, Int) -> [[Int]] -> Int -> [[Int]]
replaceIndex (row, col) board val = first ++ nrow : (tail last)
                                       where (first, last) = splitAt row board
                                             nrow = rowFst ++ val : (tail rowLst)
                                                where (rowFst, rowLst) = splitAt col (head last)

solveIndex :: (Int, Int) -> [[Int]] -> Maybe [[Int]]
solveIndex rc board = go rc board [1..9]
                                 where 
                                    go :: (Int, Int) -> [[Int]] -> [Int] -> Maybe [[Int]]
                                    go _ _ [] = Nothing
                                    go rc board (v:vs) = 
                                       if (boardValid new_board)
                                          then let result = recursiveBacktracking new_board (findEmpty new_board)
                                                in case result of
                                                     Just res_board -> result
                                                     Nothing -> go rc board vs
                                          else go rc board vs
                                       where new_board = replaceIndex rc board v

enumerate :: [a] -> [(Int, a)]
enumerate ls = go ls 0
                  where go :: [a] -> Int -> [(Int, a)]
                        go (x:xs) i = (i, x) : (go xs (i + 1))
                        go _ _  = []

emptyIndices :: (Int, [Int]) -> [(Int, Int)]
emptyIndices (row, xs) = foldl (\ls (col, x) -> if x == 0 then (row, col):ls else ls) [] (enumerate xs)

findEmpty :: [[Int]] -> [(Int, Int)]
findEmpty board = concat $ map (emptyIndices) (enumerate board)

recursiveBacktracking :: [[Int]] -> [(Int, Int)] -> Maybe [[Int]]
recursiveBacktracking board [] = if boardComplete board then Just board else Nothing
recursiveBacktracking board empty@(i:xi) = let new_board = solveIndex i board
                                             in case new_board of
                                                  Just nb -> recursiveBacktracking nb xi
                                                  Nothing -> Nothing

solveBoard :: [[Int]] -> Maybe [[Int]]
solveBoard board = recursiveBacktracking board (findEmpty board)

readBoard :: String -> [[Int]]
readBoard [] = []
readBoard input = (readRow $ take 9 input) : (readBoard $ drop 9 input)
                     where readRow row = [if ch == '.' then 0 else digitToInt ch | ch <- row]

main = do
   args <- getArgs
   if (length $ head args) /= 81 
      then error "invalid board length"
      else do
         let board = readBoard (head args)
         let solution = case solveBoard board of
                       Just solved -> showBoard solved
                       Nothing -> "No solution found"
         putStr $ solution
