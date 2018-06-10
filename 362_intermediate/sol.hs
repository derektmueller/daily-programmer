module Main where

import Data.Char
import Data.List
import Debug.Trace
import Control.Monad.Trans.State

data Direction = Clockwise | Counterclockwise deriving Show

type Dimensions = (Int, Int)
type Message = String
type Grid = [String]

data Problem = Problem {
  getMessage :: Message,
  getDimensions :: Dimensions,
  getDirection :: Direction
} deriving Show

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

reflectY :: [[a]] -> [[a]]
reflectY = transpose . reverse . transpose

input = [
    Problem "WE ARE DISCOVERED. FLEE AT ONCE" (9, 3) Clockwise,
    Problem 
      "why is this professor so boring omg" (6, 5) Counterclockwise,
    Problem 
      "Solving challenges on r/dailyprogrammer is so much fun!!" 
      (8, 6) Counterclockwise,
    Problem 
      "For lunch let's have peanut-butter and bologna sandwiches" 
      (4, 12) Clockwise,
    Problem 
      "I've even witnessed a grown man satisfy a camel" 
      (9,5) Clockwise,
    Problem 
      "Why does it say paper jam when there is no paper jam?" 
      (3, 14) Counterclockwise
  ]

mkGrid :: Dimensions -> Message -> Grid
mkGrid (x, y) message = mkGrid' (x, y) message []
  where
    mkGrid' :: Dimensions -> Message -> Grid -> Grid
    mkGrid' (_, 0) _ grid = grid
    mkGrid' (x, y) message grid = 
      mkGrid' (x, y - 1) (drop x message) (grid ++ [take x message])

fmt :: Message -> Message
fmt = (filter (flip elem (['a'..'z'] ++ ['A'..'Z']))) . (fmap toUpper)

pad :: Dimensions -> Message -> Message
pad (x, y) str 
  | x * y > length str = str ++ (replicate (x * y - length str) 'X')
  | otherwise = str

encrypt :: Direction -> Grid -> String
encrypt Clockwise grid = 
  evalState (encrypt'' grid) ""
encrypt Counterclockwise grid = 
  evalState (encrypt'' ((rotateRight . reflectY) grid)) ""

encrypt' :: Grid -> State String String
encrypt' grid = do
  acc <- get
  if grid /= []
  then do
    let grid' = rotateLeft grid
    put (acc ++ (head grid'))
    encrypt' (tail grid')
  else do
    return acc

solve :: Problem -> String
solve (Problem message dimensions direction) = 
  encrypt 
    direction 
    (mkGrid 
      dimensions 
      (((pad dimensions) . fmt) message))

main = do
  mapM_ (putStrLn . solve) input
