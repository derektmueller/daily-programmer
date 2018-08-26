module Lib where

import Data.List
import qualified Data.Set as S
import System.IO
import Test.Hspec
import Control.Monad.IO.Class
import Safe.Foldable
import Data.Foldable
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import qualified Data.Map as M
import Debug.Trace

type Funnel3 a = ReaderT FunnelConfig (State FunnelState) a 

data Entry = Entry {
  smallest :: Maybe Int,
  largestQuery :: Maybe Int
}

data FunnelState = FunnelState {
  cache :: M.Map String Entry
}

data FunnelConfig = FunnelConfig {
  wordSet :: S.Set String,
  wordsByLength :: M.Map Int [String]
}

candidateWords :: String -> [String]
candidateWords word = 
  nub (fmap (\x -> (fst x ++ snd x)) $ 
    zip (inits word) (drop 1 $ tails word)) 

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

choose :: [b] -> Int -> [[b]]
_ `choose` 0 = [[]]
[] `choose` _ =  []
(x:xs) `choose` k = (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

cacheHit :: Int -> Entry -> Bool
cacheHit _ (Entry Nothing Nothing) = False
cacheHit val (Entry (Just smallest) _) = val <= smallest
cacheHit val (Entry _ (Just largestQuery)) = val >= largestQuery

hasFunnel :: Int -> Entry -> Bool
hasFunnel val (Entry (Just smallest) _) = val <= smallest
hasFunnel val _ = False

updateSmallest :: Entry -> Int -> Entry
updateSmallest (Entry Nothing largestQuery) n = 
  Entry (Just n) largestQuery
updateSmallest e@(Entry (Just smallest) largestQuery) n
  | n < smallest = Entry (Just n) largestQuery
  | otherwise = e

updateLargestQuery :: Entry -> Int -> Entry
updateLargestQuery (Entry smallest Nothing) n = 
  Entry smallest (Just n)
updateLargestQuery e@(Entry smallest (Just largestQuery)) n
  | n < largestQuery = Entry smallest (Just n)
  | otherwise = e

updateEntry :: 
  Bool -> String -> Int -> Maybe Entry -> Funnel3 ()
updateEntry hasFunnel word size maybeEntry = do
  state <- lift get
  let entries = cache state
  case maybeEntry of
    Nothing ->
      case hasFunnel of
        True -> do
          lift $ put $ state {
            cache = 
              (M.insert word (Entry (Just size) Nothing) entries)}
        False -> do
          lift $ put $ state {
            cache = 
              (M.insert word (Entry Nothing (Just size)) entries)}
    (Just entry) -> do
      case hasFunnel of
        True -> do
          lift $ put $ state {
            cache = 
              (M.insert word (updateSmallest entry size) entries)}
        False -> do
          lift $ put $ state {cache = (M.insert word 
              (updateLargestQuery entry size) entries)}

mkFunnelConfig :: S.Set String -> [String] -> FunnelConfig
mkFunnelConfig aWordSet wordList = 
  FunnelConfig aWordSet (mkWordsByLength wordList)

initFunnelBState :: FunnelState
initFunnelBState = FunnelState M.empty

mkWordsByLength :: [String] -> M.Map Int [String]
mkWordsByLength wordList = 
  foldr (\word acc -> 
    M.alter (\maybeCount ->
      case maybeCount of
        Nothing -> Just [word]
        Just xs -> Just $ word:xs
      ) (length word) acc
  ) M.empty wordList

containsWord :: String -> String -> Bool
containsWord [] _ = True
containsWord _ [] = False
containsWord l@(x:xs) (y:ys)
  | x == y = containsWord xs ys
  | otherwise = containsWord l ys

funnel3 :: String -> Int -> Funnel3 Bool
funnel3 word size = do
  aWordSet <- asks wordSet
  aWordsByLength <- asks wordsByLength
  if size == 1
  then
    return True
  else if length word < size
  then
    return False
  else do
    entries <- cache <$> lift get
    let maybeEntry = M.lookup word entries
    case maybe False (cacheHit size) maybeEntry of
      True ->
        return $ maybe False (hasFunnel size) maybeEntry
      False -> do
        let nextWords = 
              maybe [] id $
              fmap (filter (flip containsWord $ word)) $
              fmap concat $
              sequenceA $
              filter (/=Nothing) $
              (\len -> M.lookup len aWordsByLength) <$>
                  [(size - 1)..(length word) - 1]
        hasFunnel <- foldlM (\acc word ->
          if acc
          then
            return acc
          else do
            hasFunnel <- funnel3 word (size - 1) 
            return hasFunnel) False nextWords
        updateEntry hasFunnel word size maybeEntry
        return hasFunnel

bonus1 :: [String] -> S.Set String -> String 
bonus1 wordList wordSet = 
  head $ 
    filter (\word -> funnel2 wordSet word == 10) wordList

bonus2 :: [String] -> Funnel3 [String]
bonus2 wordList = do
  hasFunnels <- traverse (\word -> funnel3 word 12) wordList
  return $ do
    (hasFunnel, word) <- (zip hasFunnels wordList)
    if hasFunnel
    then
       return word
    else
      []

test = hspec $ do
  xdescribe "bonus2" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let wordList = 
            words contents  
          wordSet = S.fromList wordList
          solution = 
            (evalState 
              (runReaderT 
                (bonus2 wordList) (mkFunnelConfig wordSet wordList))
              initFunnelBState)
      mapM_ putStrLn solution
      length solution `shouldBe` 6
  describe "funnel3" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let wordList = words contents  
          wordSet = S.fromList wordList
      (evalState 
        (runReaderT 
          (funnel3 "preformationists" 12) 
          (mkFunnelConfig wordSet wordList))
        initFunnelBState)
        `shouldBe` True
  describe "bonus1" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let wordList = words contents  
      bonus1 wordList (S.fromList wordList) `shouldBe` "complecting"
  describe "funnel2" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let wordSet = S.fromList $ words contents  
      funnel2 wordSet "gnash" `shouldBe` 4
      funnel2 wordSet "princesses" `shouldBe` 9
      funnel2 wordSet "turntables" `shouldBe` 5
      funnel2 wordSet "implosive" `shouldBe` 1
      funnel2 wordSet "programmer" `shouldBe` 2
  describe "bonus366B" $ do
    it "works" $ do
      contents <- liftIO $ readFile "./enable1.txt"
      let wordSet = S.fromList $ words contents  
      bonus366B wordSet "dragoon" `shouldBe` ["dragon"]
      bonus366B wordSet "boats" `shouldBe` ["oats", "bats", "bots", "boas", "boat"]
      bonus366B wordSet "affidavit" `shouldBe` []
