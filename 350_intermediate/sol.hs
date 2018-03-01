#!/usr/bin/env stack
-- stack --resolver lts-6.15 script

import System.Environment   
import System.IO 
import Data.List
import Data.List.Split

parseInput :: String -> [[Integer]]
parseInput str = 
  getProblems lines
  where
    lines = init $ splitOn "\n" $ str
    getProblems [] = []
    getProblems lines = 
      map read (splitOn " " (lines !! 1)) : 
        getProblems (drop 2 lines)

solve :: [Integer] -> [Integer]
solve = solve' 0
  where
    solve' :: Int -> [Integer] -> [Integer]
    solve' i debits
      | i < length debits =
        if foldr (+) 0 (take i debits) == 
          foldr (+) 0 (drop (i + 1) debits) then
          toInteger i : solve' (i + 1) debits
        else
          solve' (i + 1) debits
      | otherwise = []

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode  
  contents <- hGetContents handle  
  let problems = parseInput contents
  putStrLn $ intercalate "\n" $ 
    map (intercalate " " . map show) $ map solve problems
  hClose handle 

