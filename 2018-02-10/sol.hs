#!/usr/bin/env stack
-- stack --resolver lts-6.15 script
--module Sol where

import System.Environment   
import System.IO 
import Data.List
import Data.List.Split

data Shelf size id = Shelf size id deriving (Show, Eq)
    
groupBooksByShelf :: 
  Integer -> ([Integer], [Integer]) -> 
  [(Integer, Shelf Integer Integer)]
groupBooksByShelf i (_, []) = []
groupBooksByShelf i ([], _) = []
groupBooksByShelf i (booksOrdering, shelf:shelves) = 
  shelvedBooks ++ 
    (groupBooksByShelf (i + 1) (remainingBooks, shelves))
  where
    booksFittingOnShelf (books, booksWidth) = booksWidth <= shelf
    shelveBook book = (book, Shelf shelf i)
    remainingBooks = filter (\book -> 
      not $ elem book (map fst shelvedBooks)) booksOrdering
    shelvedBooks =  
      (map shelveBook $
        maybe [] fst $ 
        find booksFittingOnShelf $ 
        reverse 
          (zip (inits booksOrdering) (map sum (inits booksOrdering))))

shelfCount :: [(Integer, Shelf Integer Integer)] -> Integer
shelfCount solution = count
  where Shelf size count = snd $ last solution

compareSolutions :: 
  [(Integer, Shelf Integer Integer)] ->
  [(Integer, Shelf Integer Integer)] -> Ordering
compareSolutions sol1 sol2 = 
  compare (shelfCount sol1) (shelfCount sol2)

filterValidSolutions ::
  [Integer] ->
  [[(Integer, Shelf Integer Integer)]] ->
  [[(Integer, Shelf Integer Integer)]]
filterValidSolutions books = filter (\solution -> 
  length solution == length books)

solution :: 
  [Integer] -> [Integer] -> Maybe [(Integer, Shelf Integer Integer)]
solution books shelves =
  getOptimalSolution validSolutions
  where
    booksPermutations = permutations books
    shelvesPermutations = permutations shelves
    booksShelvesOrderings = [(books, shelves) |
      books <- booksPermutations, shelves <- shelvesPermutations]
    validSolutions = 
      filterValidSolutions books $
      map (groupBooksByShelf 1) booksShelvesOrderings
    getOptimalSolution [] = Nothing
    getOptimalSolution solutions = 
      Just $ minimumBy compareSolutions solutions

printSolution :: Maybe [(Integer, Shelf Integer Integer)] -> IO ()
printSolution Nothing = putStrLn "impossible"  
printSolution (Just solution) = putStrLn $ show $ shelfCount solution

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode  
  contents <- hGetContents handle  
  let lines = init $ splitOn "\n" $ contents
  let shelves = map read $ splitOn " " $ head lines :: [Integer]
  let books = map (read . head . words) $ tail lines :: [Integer]
  printSolution $ solution books shelves
  hClose handle 
