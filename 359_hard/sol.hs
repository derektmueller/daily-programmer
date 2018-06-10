module Main where

input = [
    [1, 1, 3],
    [7, 5, 4],
    [9, 3, 7]]

isPrime :: Integer -> Bool
isPrime k = 
  null [ x | 
         x <- [2..(floor (sqrt (fromInteger k)))], k `mod` x  == 0]

solve :: [[Integer]] -> [Integer]
solve input = do
  (y, row) <- zip [0..] input
  x <- row
  solve' input x y

solve' :: [[Integer]] -> Integer -> Integer -> [Integer]
solve' input x y = undefined

main = do
  putStrLn "sol"
