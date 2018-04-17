module Sol where

--pumpkin, apple, eggs, milk, sugar

maxPies :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxPies (a, b) (c, d)
  | a + b >= c + d = (a, b)
  | a + b < c + d = (c, d)

bakePumpkinPie :: [Int] -> [Int]
bakePumpkinPie ingredients = zipWith (-) ingredients [1, 0, 3, 4, 3]

bakeApplePie :: [Int] -> [Int]
bakeApplePie ingredients = zipWith (-) ingredients [0, 1, 4, 3, 2]

canBakePumpkinPie :: [Int] -> Bool
canBakePumpkinPie ingredients = (length $ filter (<0) $ bakePumpkinPie ingredients) == 0

canBakeApplePie :: [Int] -> Bool
canBakeApplePie ingredients = (length $ filter (<0) $ bakeApplePie ingredients) == 0

solve :: [Int] -> (Int, Int)
solve ingredients = solve' ingredients (0, 0)

solve' :: [Int] -> (Int, Int) -> (Int, Int)
solve' ingredients (pumpkinPies, applePies) = 
  if canBakeApplePie ingredients && canBakePumpkinPie ingredients
  then
    maxPies 
      (solve' (bakePumpkinPie ingredients) 
        (pumpkinPies + 1, applePies))
      (solve' (bakeApplePie ingredients) 
        (pumpkinPies, applePies + 1))
  else if canBakeApplePie ingredients
  then
    solve' (bakeApplePie ingredients) (pumpkinPies, applePies + 1)
  else if canBakeApplePie ingredients
  then
    solve' (bakePumpkinPie ingredients) (pumpkinPies + 1, applePies)
  else
    (pumpkinPies, applePies)

printSolution (pumpkinPies, applePies) = do
  print $
    (show pumpkinPies) ++ " pumpkin pies and " ++ 
    (show applePies) ++ " apple pies"

main = do
  printSolution $ solve [10, 14, 10, 42, 24]
  printSolution $ solve [12, 4, 40, 30, 40]
  printSolution $ solve [12, 14, 20, 42, 24]
