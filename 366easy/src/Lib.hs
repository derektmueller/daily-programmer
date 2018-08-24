module Lib where

import Data.List
import qualified Data.Set as S
import System.IO
import Test.Hspec
import Control.Monad.IO.Class

candidateWords :: String -> [String]
candidateWords word = nub (fmap (\x -> (fst x ++ snd x)) $ zip (inits word) (drop 1 $ tails word)) 

funnel :: String -> String -> Bool
funnel word1 word2 = 
  word2 `elem` candidateWords word1

bonus :: S.Set String -> String -> [String]
bonus enable1Set word = filter (flip S.member enable1Set) $ candidateWords word 

bonus2 :: [String] -> [String]
bonus2 enable1 = filter (\word -> length (bonus enable1Set word) == 5) enable1
  where
    enable1Set = S.fromList enable1

test = hspec $ do
  describe "funnel" $ do
    it "works" $ do
      funnel "leave" "eave" `shouldBe` True
      funnel "reset" "rest" `shouldBe` True
      funnel "dragoon" "dragon" `shouldBe` True
      funnel "eave" "leave" `shouldBe` False
      funnel "sleet" "lets" `shouldBe` False
      funnel "skiff" "ski" `shouldBe` False
  describe "bonus" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let enable1Words = S.fromList $ words contents  
      bonus enable1Words "dragoon" `shouldBe` ["dragon"]
      bonus enable1Words "boats" `shouldBe` ["oats", "bats", "bots", "boas", "boat"]
      bonus enable1Words "affidavit" `shouldBe` []
  describe "bonus2" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let enable1Words = words contents  
      length (bonus2 enable1Words) `shouldBe` 28
