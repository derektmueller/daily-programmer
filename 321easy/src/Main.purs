module Main where

import Prelude
import Data.Decimal (Decimal, fromInt)
import Effect (Effect)
import Effect.Console (log)

two = fromInt 2

subfactorial :: Decimal -> Decimal
subfactorial n 
  | n == fromInt 0 = one
  | n == fromInt 1 = zero
  | n == fromInt 2 = one
  | otherwise = 
    (n `sub` one) * (subfactorial (n `sub` one) + subfactorial (n `sub` two))

main :: Effect Unit
main = do
  log $ show $ subfactorial (fromInt 5)
  log $ show $ subfactorial (fromInt 6)
  log $ show $ subfactorial (fromInt 9)
  log $ show $ subfactorial (fromInt 14)
