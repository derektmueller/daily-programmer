module Lib where

import Data.List (transpose)

revenue = [
    [120, 145],
    [243, 265]
  ] :: [[Double]]

expenses = [
    [130, 59],
    [143, 198]
  ] :: [[Double]]

revenue' = [
    [190, 140, 1926, 14, 143],
    [325, 19, 293, 1491, 162],
    [682, 14, 852, 56, 659],
    [829, 140, 609, 120, 87]
  ]

expenses' = [
    [120, 65, 890, 54, 430],
    [300, 10, 23, 802, 235],
    [50, 299, 1290, 12, 145],
    [67, 254, 89, 129, 76]
  ]

commissionRate = 0.062

commission :: [[Double]] -> [[Double]] -> [Double]
commission rev exp = 
  fmap (*commissionRate) $
    fmap sum $
      (fmap.fmap) (\x -> if x > 0 then x else 0) $
        zipWith (zipWith (-)) (transpose rev) (transpose exp)

solve :: IO ()
solve = do
  putStrLn $ show $ commission revenue expenses
  putStrLn $ show $ commission revenue' expenses'
