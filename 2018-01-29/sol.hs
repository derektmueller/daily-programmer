#!/usr/bin/env stack
-- stack --resolver lts-6.15 script
module Sol where

import System.Environment   
import System.IO 
import Data.List
import Data.List.Split

data Problem = Problem Int [Int] (Int -> Bool)
data Solution = Solution (Maybe [Int]) deriving Show

combinations :: [Int] -> Int -> [[Int]]
combinations origXs origK = combinations' origXs origK
  where
    combinations' xs k = list !! ((((length origXs) + 1) * k + ((length xs) + 1)) - 1)
      where 
        list = (map (uncurry combinations'') (concat [zip (reverse $ tails origXs) (repeat x) | x <- [0..origK]])) 
        combinations'' _ 0 = [[]]
        combinations'' [] _ = []
        combinations'' (x:xs) k = 
          map head $ group $ sort ((map (x:) (combinations' xs (k - 1))) ++ (combinations' xs k))
          --((map (x:) (combinations' xs (k - 1))) ++ (combinations' xs k))

subsets :: [Int] -> [[Int]]
subsets xs = concat [combinations xs n | n <- [1..length xs]]

parseProblemOutput :: String -> (Int -> Bool)
parseProblemOutput s =
  case operatorStr of
    ">" -> (> value)
    "<" -> (< value)
    "<=" -> (<= value)
    ">=" -> (>= value)
  where
    [_, operatorStr, valueStr] = (tail (splitOn " " s))
    value = read valueStr

parseProblem :: String -> Problem
parseProblem s = do
  Problem 
    (read (splitOn " " input !! 1)) 
    (map read (drop 2 (splitOn " " input)))
    (parseProblemOutput output)
  where
    lines = splitOn "\n" s
    [input, output] = take 2 lines

parseInput :: String -> [Problem]
parseInput s = 
  map parseProblem problems
  where problems = splitOn "\n\n" s

solve :: Problem -> Solution
solve p = do
  if length solutions > 0 then
    Solution (Just (head solutions))
  else
    Solution Nothing
  where
    Problem total coins condition = p
    possibleSolutions = subsets coins
    solutions = 
      (filter (\x -> 
        sum x == total && condition (length x)) possibleSolutions)

main = do
--  args <- getArgs
--  handle <- openFile (head args) ReadMode  
--  contents <- hGetContents handle  
--  let problems = parseInput contents
--  let solutions = map solve problems
--  putStrLn (intercalate "\n" (map (\x -> case x of
--    (Solution (Just coins)) -> intercalate " " (map show coins)
--    (Solution Nothing) -> "No solution.") solutions))
--  hClose handle 
  print $ take 10 $ combinations (take 100 (repeat 1)) 99 
