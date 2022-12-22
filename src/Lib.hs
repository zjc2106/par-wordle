module Lib
    ( playPar,
      playSeq,
      getValidWords,
      initKnowledge
      )
where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import Control.Parallel.Strategies

data Knowledge = Knowledge { green :: Set.Set (Int, Char)
                   , yellow :: Set.Set (Int, Char),
                   grey :: Set.Set Char } deriving (Eq, Ord, Show)

initKnowledge :: Knowledge
initKnowledge = Knowledge Set.empty Set.empty Set.empty

--play game in parallel
playPar :: B.ByteString -> B.ByteString -> Knowledge -> Set.Set B.ByteString -> Int -> IO ()
playPar g a k gs gCount = do
   if g == a then do
      return ()
   else do
      let k' = addToKnowledge k g a
          gs' = Set.delete g gs
          g' = fst $ maxEntropyPar gs' k'
      print g'
      playPar g' a k' gs' (gCount+1)

-- play game sequentially
playSeq :: B.ByteString -> B.ByteString -> Knowledge -> Set.Set B.ByteString -> Int -> IO ()
playSeq g a k gs gCount = do
   if g == a then do
      return ()
   else do
      let k' = addToKnowledge k g a
          gs' = Set.delete g gs
          g' = fst $ maxEntropy gs' k'
      print g'
      playSeq g' a k' gs' (gCount+1)

{-
Converts a set of ByteStrings to a Set of words of length = 5.
-}
getValidWords:: [B.ByteString] -> Set.Set B.ByteString
getValidWords = Set.fromList . filter (\x -> B.length x == 5)

{-
Takes an array of (index, character) and a Set of ByteString. Filters the Set to only include words that
contain character at index.
-}
matchGreens:: [(Int, Char)] -> Set.Set B.ByteString ->Set.Set B.ByteString
matchGreens [] w = w
matchGreens ((i,c):xs) w = Set.intersection (Set.filter (\x -> B.index x i == c) w) (matchGreens xs w)

{-
Takes an array of characters and a Set of ByteString. Filters the Set to only include words that
include all characters.
-}
matchYellows:: [(Int, Char)] -> Set.Set B.ByteString -> Set.Set B.ByteString
matchYellows [] w = w
matchYellows ((i, c):xs) w = Set.intersection (Set.intersection (Set.filter (B.elem c) w) (Set.filter (\x -> B.index x i /= c) w)) (matchYellows xs w)

{-
Takes an array of characters and a Set of ByteString. Filters the Set to only include words that
do not include any of the characters.
-}
matchGreys:: String -> Set.Set B.ByteString -> Set.Set B.ByteString
matchGreys xs w
  = foldr (\ x -> Set.intersection (Set.filter (B.notElem x) w)) w xs

{-
Takes a guess and returns a Set of valid possible words.
-}
possibleWords:: Lib.Knowledge -> Set.Set B.ByteString -> Set.Set B.ByteString
possibleWords k g = Set.intersection (Set.intersection (matchGreens (Set.toList (green k)) g) (matchYellows (Set.toList (yellow k)) g)) (matchGreys (Set.toList (grey k)) g)

-- Computes the yellow letters of a guess for a given answer 
getYellows :: B.ByteString -> B.ByteString -> Set.Set (Int, Char)
getYellows g a = l `Set.difference` getGreens g a
   where s = Set.fromList $ B.unpack a
         l = Set.fromList $ filter (\x -> snd x `Set.member` s) ([0..] `zip` B.unpack g)

-- Computes the green letters of a guess for a given answer 
getGreens:: B.ByteString -> B.ByteString -> Set.Set (Int, Char)
getGreens g a = Set.fromList ([0..] `zip` B.unpack g) `Set.intersection` Set.fromList ([0..] `zip` B.unpack a)

-- Computes the grey letters of a guess for a given answer 
getGreys:: B.ByteString -> B.ByteString -> Set.Set Char
getGreys g a = s `Set.difference` ng
   where s = Set.fromList $ B.unpack g
         ng = Set.map snd (getGreens g a) `Set.union` Set.map snd (getYellows g a)

-- Updates knowledge for a guess-answer pair
addToKnowledge :: Knowledge -> B.ByteString -> B.ByteString -> Knowledge
addToKnowledge k g a = Knowledge greens yellows greys
   where greens = green k `Set.union` getGreens g a
         yellows = yellow k `Set.union` getYellows g a
         greys = grey k `Set.union` getGreys g a

-- Creates a frequency map from a list
count :: Ord a => [a] -> Map.Map a Float
count = Map.fromListWith (+) . (`zip` repeat 1)

-- computes the entropy of a guess
entropy :: B.ByteString -> Knowledge -> Set.Set B.ByteString ->Float
entropy g k as = Map.foldl (+) 0 $ Map.unionWith (*) p inf
   where l = fromIntegral (Set.size as)
         p =  Map.map (/l) $ count (map (addToKnowledge k g) (Set.toList as))
         inf = Map.map (\x ->logBase 2 (1/x)) p

-- parallel computes the entropies of all guesses
entropiesPar :: Set.Set B.ByteString -> Knowledge -> [(B.ByteString, Float)]
entropiesPar gs k = e_list
   where g_list = Set.toList gs
         as = possibleWords k gs
         e_list = map (\g -> (g, entropy g k as)) g_list `using` parBuffer 200 rdeepseq

-- sequentially computes the entropies of all guesses
entropies :: Set.Set B.ByteString -> Knowledge -> [(B.ByteString, Float)]
entropies gs k = e_list
   where g_list = Set.toList gs
         as = possibleWords k gs
         e_list = map (\g -> (g, entropy g k as)) g_list

maxEntropyHelper :: (B.ByteString, Float) -> [(B.ByteString, Float)] -> (B.ByteString, Float)
maxEntropyHelper m []  = m
maxEntropyHelper (max_guess, max_entr) ((guess, entr):xs)
   | entr > max_entr = maxEntropyHelper (guess, entr) xs
   | otherwise = maxEntropyHelper (max_guess, max_entr) xs

-- parallel computes the maximum entropy of all guesses
maxEntropyPar :: Set.Set B.ByteString -> Knowledge ->  (B.ByteString, Float)
maxEntropyPar gs k
   | Set.size as == 1 = (Set.elemAt 0 as, 0)
   | otherwise = maxEntropyHelper (B.empty, -1) (entropiesPar gs k)
   where as = possibleWords k gs

-- sequentially computes the maximum entropy of all guesses
maxEntropy :: Set.Set B.ByteString -> Knowledge ->  (B.ByteString, Float)
maxEntropy gs k
   | Set.size as == 1 = (Set.elemAt 0 as, 0)
   | otherwise = maxEntropyHelper (B.empty, -1) (entropies gs k)
   where as = possibleWords k gs

