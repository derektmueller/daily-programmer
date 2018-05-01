module Main where

rps :: Integer -> [Integer]
rps 1 = [1]
rps n = 1 : (concat $ zipWith (\x y -> [x, y]) 
  (rps (n - 1)) (cycle [0, 1]))

main = do
  let n = 8
  putStrLn $ concat $ map show $ rps n
