module Main where

import System.Environment
import System.IO
import Data.List
import Data.List.Split
import Control.Monad
import Debug.Trace

type Piece = (Int, Int, Int, Int, Int)

data Problem = Problem Int Int [Piece] deriving Show

removePiece :: [Piece] -> Piece -> [Piece]
removePiece pieces piece = filter (/=piece) pieces

index :: Piece -> Int
index (i, _, _, _, _) = i

left :: Piece -> Int
left (_, _, _, _, l) = l

right :: Piece -> Int
right (_, _, r, _, _) = r

bottom :: Piece -> Int
bottom (_, _, _, b, _) = b

top :: Piece -> Int
top (_, t, _, _, _) = t

solve :: Int -> Int -> [Piece] -> [[Piece]]
solve w h pieces = reshape w h (head (solve' 0 [] w h pieces))

solve' :: 
  Int -> [(Int, Int, Int, Int, Int)] -> Int -> Int ->
  [(Int, Int, Int, Int, Int)] -> [[(Int, Int, Int, Int, Int)]]
--solve' x solution w h pieces | 
  --trace ("solve' " ++ (show solution)) False = undefined
solve' x solution w h pieces =
  if x >= (w * h) 
  then do
    return solution
  else do
    let y = x `div` w
    piece <- nub pieces
    let (i, t, r, b, l) = piece
    guard 
      (if x `mod` w == 0
      then
        l == 0
      else
        (-l) == (right $ solution !! (x - 1)))
    guard 
      (if x `mod` w == (w - 1)
      then
        r == 0
      else
        True)
    guard 
      (if y == 0
      then
        t == 0
      else
        (-t) == (bottom $ solution !! (x - w)))
    guard 
      (if y == (h - 1)
      then
        b == 0
      else
        True)
    solve' 
      (x + 1) (solution ++ [piece]) 
      w h (removePiece pieces piece)

reshape :: Int -> Int -> [Piece] -> [[Piece]]
reshape w h solution = 
  map (\i -> (take w).(drop (w * i)) $ solution) [0..(w - 1)]

printSol :: [[Piece]] -> IO ()
--printSol solution | 
  --trace ("trace " ++ (show solution)) False = undefined
printSol solution = do
  mapM_ putStrLn $ map (concat.(intersperse " ")) $
    ((map.map) (show.index) (solution))

parseFile :: String -> Problem
parseFile contents =
  let 
    dimensions:pieces = lines contents
    parsedDimensions = map read $ splitOn ", " dimensions 
    parsedPieces = map parsePiece pieces
    in
    Problem (parsedDimensions !! 0) (parsedDimensions !! 1)
      parsedPieces

parsePiece :: String -> Piece
parsePiece piece = 
  let parts = 
        (map (read :: String -> Int) $ 
        (wordsBy 
          (\delim -> delim `elem` [':', ',']) piece)) :: [Int] in
        (parts !! 0, 
         parts !! 1, 
         parts !! 2, 
         parts !! 3, 
         parts !! 4)

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode  
  contents <- hGetContents handle  
  let Problem x y pieces = parseFile contents
  let solution = solve x y pieces
  printSol solution
