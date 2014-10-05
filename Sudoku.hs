-- Sudoku solver
-- The number 0 is used to represent unknown values, I.E. empty boxes

module Sudoku where

import Data.Char
import Data.List

type Sudoku = [Int]

emptySudoku :: Sudoku
emptySudoku = mkSudoku $ replicate 81 '0'

mkSudoku :: String -> Sudoku
mkSudoku = map digitToInt

validSudoku :: Sudoku -> Bool
validSudoku sud = all id $ map allUnique $ rows sud ++ cols sud ++ squares sud

showSudoku :: Sudoku -> String
showSudoku sud = unlines $ first3 ++ ["------+-------+------"] ++ second3 ++ ["------+-------+------"] ++ third3
  where
    rowStrs = map showLine $ rows sud
    showLine = unwords . addColumns . map show
    addColumns (n1:n2:n3:n4:n5:n6:n7:n8:n9:[]) = n1:n2:n3:"|":n4:n5:n6:"|":n7:n8:n9:[]
    first3 = take 3 rowStrs
    second3 = take 3 (drop 3 rowStrs)
    third3  = drop 6 rowStrs

rows :: Sudoku -> [[Int]]
rows = splitEvery 9

cols :: Sudoku -> [[Int]]
cols = transpose . rows

triples :: Sudoku -> [[(Int, Int, Int)]]
triples sud = [zip3 c1 c2 c3] ++ [zip3 c4 c5 c6] ++ [zip3 c7 c8 c9]
  where
    (c1:c2:c3:c4:c5:c6:c7:c8:c9:[]) = cols sud

squares :: Sudoku -> [[Int]]
squares = concatMap (splitEvery 9 . squares') . triples
  where
    squares' = foldr fn []
    fn (a,b,c) acc = a:b:c:acc

splitEvery :: Int -> [a] -> [[a]]
splitEvery 0 xs = [xs]
splitEvery _ [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)

allUnique :: [Int] -> Bool
allUnique [] = True
allUnique (x:xs) = (x == 0 || not (x `elem` xs)) && allUnique xs
