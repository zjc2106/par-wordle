module Main
   ( main )
where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import qualified System.Environment as Env
import qualified System.Exit as Exit
import Control.Parallel.Strategies

main :: IO ()
main = do
   args <- Env.getArgs
   if length args /= 2 then do
      n <- Env.getProgName
      Exit.die $ "Usage: " ++ n ++ "<answer> <filename>"
   else do
      let a = head args
      if length a /= 5 then do
         Exit.die "Starting guess must be length 5"
      else do
         let f = args !! 1
         f' <- B.readFile f
         let gSet = (getValidWords . B.words) f'
             k = initKnowledge
             a' = B.pack a
             g = B.pack "raise"
         print g
         play g a' k gSet 1


data Knowledge = Knowledge { green :: Set.Set (Int, Char)
                   , yellow :: Set.Set (Int, Char),
                   grey :: Set.Set Char } deriving (Eq, Ord, Show)

initKnowledge :: Knowledge
initKnowledge = Knowledge Set.empty Set.empty Set.empty


play :: B.ByteString -> B.ByteString -> Knowledge -> Set.Set B.ByteString -> Int -> IO ()
play g a k gs gCount = do
   if g == a then do
      return ()
   else do
      let k' = addToKnowledge k g a
          gs' = Set.delete g gs 
          g' = fst $ maxEntropy gs' k'
      print g'
      play g' a k' gs' (gCount+1)

-- play :: B.ByteString -> B.ByteString -> Knowledge -> Set.Set B.ByteString -> Int -> Int
-- play g a k gs count
--    | g == a = count + 1
--    | otherwise = play g' a k' gs' (count+1)
--    where k' = addToKnowledge k g a
--          gs' = Set.delete g gs 
--          g' = fst $ maxEntropy gs' k'


{-
Converts a set of ByteStrings to a Set of words of length = 5.
-}
getValidWords:: [B.ByteString] -> Set.Set B.ByteString
getValidWords = Set.fromList . filter (\x -> B.length x == 5)

{-
Takes an array of (index, character) and a Set of ByteString. Filters the Set to only include words that
contain character at index.
-}
-- matchGreens:: Set.Set (Int, Char) -> Set.Set B.ByteString ->Set.Set B.ByteString
-- matchGreens set w
--    | set == Set.null = w
--    | otherwise = Set.intersection (Set.filter (\x -> B.index x (set.elemAt 0) == (set.elemAt 1)) w) (matchGreens newSet w)
--    where newSet = snd (Set.splitAt 2)


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
possibleWords:: Main.Knowledge -> Set.Set B.ByteString -> Set.Set B.ByteString
possibleWords k w = Set.intersection (Set.intersection (matchGreens (Set.toList (green k)) w) (matchYellows (Set.toList (yellow k)) w)) (matchGreys (Set.toList (grey k)) w)

-- getYellowsHelper:: Int -> B.ByteString -> B.ByteString -> [(Int, Char)]
-- getYellowsHelper pos g a
--    | g == B.empty = []
--    | B.head g `B.elem` (x `B.append` B.tail y) = (pos, B.head g): getYellowsHelper (pos+1) (B.tail g) a
--    | otherwise = getYellowsHelper (pos+1) (B.tail g) a
--    where (x, y) = B.splitAt pos a


-- getYellows:: B.ByteString -> B.ByteString -> [(Int, Char)]
-- getYellows = getYellowsHelper 0

getYellows :: B.ByteString -> B.ByteString -> Set.Set (Int, Char)
getYellows g a = l `Set.difference` getGreens g a
   where s = Set.fromList $ B.unpack a
         l = Set.fromList $ filter (\x -> snd x `Set.member` s) ([0..] `zip` B.unpack g)

getGreens:: B.ByteString -> B.ByteString -> Set.Set (Int, Char)
getGreens g a = Set.fromList ([0..] `zip` B.unpack g) `Set.intersection` Set.fromList ([0..] `zip` B.unpack a)

getGreys:: B.ByteString -> B.ByteString -> Set.Set Char
getGreys g a = s `Set.difference` ng
   where s = Set.fromList $ B.unpack g
         ng = Set.map snd (getGreens g a) `Set.union` Set.map snd (getYellows g a)

addToKnowledge :: Knowledge -> B.ByteString -> B.ByteString -> Knowledge
addToKnowledge k g a = Knowledge greens yellows greys
   where greens = green k `Set.union` getGreens g a
         yellows = yellow k `Set.union` getYellows g a
         greys = grey k `Set.union` getGreys g a

{-
g is the guess, w is the set of possible words at this point
-}

count :: Ord a => [a] -> Map.Map a Float
count = Map.fromListWith (+) . (`zip` repeat 1)

entropy :: B.ByteString -> Knowledge -> Set.Set B.ByteString ->Float
entropy g k as = Map.foldl (+) 0 $ Map.unionWith (*) p inf
   where l = fromIntegral (Set.size as)
         p =  Map.map (/l) $ count (map (addToKnowledge k g) (Set.toList as))
         -- `using` parList rseq)
         inf = Map.map (\x ->logBase 2 (1/x)) p

entropies :: Set.Set B.ByteString -> Knowledge -> [(B.ByteString, Float)]
entropies gs k = e_list
   where g_list = Set.toList gs
         as = possibleWords k gs
         e_list = map (\g -> (g, entropy g k as)) g_list `using` parBuffer 200 rdeepseq
maxEntropyHelper :: (B.ByteString, Float) -> [(B.ByteString, Float)] -> (B.ByteString, Float)
maxEntropyHelper m []  = m
maxEntropyHelper (max_guess, max_entr) ((guess, entr):xs)
   | entr > max_entr = maxEntropyHelper (guess, entr) xs
   | otherwise = maxEntropyHelper (max_guess, max_entr) xs

maxEntropy :: Set.Set B.ByteString -> Knowledge ->  (B.ByteString, Float)
maxEntropy gs k 
   | Set.size as == 1 = (Set.elemAt 0 as, 0)
   | otherwise = maxEntropyHelper (B.empty, -1) (entropies gs k)
   where as = possibleWords k gs
