#!/usr/bin/env stack
-- stack --resolver lts-6.15 script

import System.Environment   
import System.IO 
import Data.List.Split

toBase62 :: Integer -> String
toBase62 base10Num
  | base10Num < 62 = [toBase62Digit base10Num]
  | base10Num >= 62 =
    (toBase62Digit remainder) : toBase62 quotient
  where
    (quotient, remainder) = (divMod base10Num 62)
    alphabet = 
      "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    toBase62Digit :: Integer -> Char
    toBase62Digit = (alphabet !!) . fromIntegral

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode  
  contents <- hGetContents handle  
  let lines = words contents 
  print $ map toBase62 (map read lines :: [Integer])
  hClose handle 

