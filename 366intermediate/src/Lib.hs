module Lib where

import Data.List
import qualified Data.Set as S
import System.IO
import Test.Hspec
import Control.Monad.IO.Class
import Safe.Foldable
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import qualified Data.Map as M

candidateWords :: String -> [String]
candidateWords word = 
  nub (fmap (\x -> (fst x ++ snd x)) $ 
    zip (inits word) (drop 1 $ tails word)) 

candidateWordsR :: Int -> String -> [String]
candidateWordsR 0 word = [word]
candidateWordsR maxDepth word =
  nub $ 
    nextWords ++ 
      (concat (candidateWordsR (maxDepth - 1) <$> nextWords))
  where
    nextWords = candidateWords word

bonus366B :: S.Set String -> String -> [String]
bonus366B enable1Set word = 
  filter (isValidWord enable1Set) $ candidateWords word 

isValidWord :: S.Set String -> String -> Bool
isValidWord enable1Set word = S.member word enable1Set

funnel2 :: S.Set String -> String -> Int
funnel2 = funnel2' 1
  where 
    funnel2' :: Int -> S.Set String -> String -> Int
    funnel2' acc wordSet word =
      maximumDef acc 
        (funnel2' (acc + 1) wordSet <$> (bonus366B wordSet word))

type Funnel3State = M.Map String Int

funnel3 :: S.Set String -> String -> State Funnel3State Int
funnel3 wordSet word = do
  state <- get
  let maybeCount = M.lookup word state
  case maybeCount of
    Nothing -> do
      let newWords = 
            filter (isValidWord wordSet) 
              (candidateWordsR 2 word)
      counts <- 
        traverse (funnel3 wordSet) newWords
      let count = 1 + (maximumDef 0 counts)
      state' <- get
      put (M.insert word count state')
      return count
    Just count ->
      return count

bonus1 :: [String] -> S.Set String -> String 
bonus1 wordList wordSet = 
  head $ 
    filter (\word -> funnel2 wordSet word == 10) wordList

bonus2 :: [String] -> S.Set String -> State Funnel3State [String]
bonus2 wordList wordSet = do
  traverse (funnel3 wordSet) wordList
  state <- get
  return $
    filter (\word -> 
      maybe False (==12) (M.lookup word state)) wordList

test = hspec $ do
  describe "bonus2" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let wordList = words contents  
          wordSet = S.fromList wordList
          solution = (evalState (bonus2 wordList wordSet) M.empty)
      mapM_ putStrLn solution
      length solution `shouldBe` 6
  xdescribe "funnel3" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let wordSet = S.fromList $ words contents  
      evalState (funnel3 wordSet "preformationists") M.empty 
        `shouldBe` 12 
  xdescribe "bonus1" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let wordList = words contents  
      bonus1 wordList (S.fromList wordList) `shouldBe` "complecting"
  xdescribe "funnel2" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let wordSet = S.fromList $ words contents  
      funnel2 wordSet "gnash" `shouldBe` 4
      funnel2 wordSet "princesses" `shouldBe` 9
      funnel2 wordSet "turntables" `shouldBe` 5
      funnel2 wordSet "implosive" `shouldBe` 1
      funnel2 wordSet "programmer" `shouldBe` 2
  xdescribe "bonus366B" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let wordSet = S.fromList $ words contents  
      bonus366B wordSet "dragoon" `shouldBe` ["dragon"]
      bonus366B wordSet "boats" `shouldBe` ["oats", "bats", "bots", "boas", "boat"]
      bonus366B wordSet "affidavit" `shouldBe` []
