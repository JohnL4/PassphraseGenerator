-- | Functions for the passphrase generator.

{-# LANGUAGE BangPatterns #-}

module PassphraseGenerator where

import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)

-- | Orders inputs by 2nd element (count), descending
countDescending :: (String,Int) -> (String,Int) -> Ordering
countDescending (_, countA) (_, countB)
  | countA < countB  = GT
  | countA == countB = EQ
  | otherwise        = LT  

-- | Returns map of all counts (summed) for words occurring on or after given year.
wordCounts :: Int               -- ^ Year
           -> [String]          -- ^ Lines in form "word\tyear\tcount\totherStuffWeDontCareAbout"
           -> Map.Map String Int -- ^ Input map
           -> Map.Map String Int -- ^ Output map
wordCounts _ [] aMap = aMap
wordCounts aYear (aLine:restLines) !aMap =
  let fields      = splitOn "\t" aLine
      ngramParts  = splitOn "_" (fields!!0)
      ngram       = map toLower (ngramParts!!0) -- "ngram" is the same as "word", in this case.
      ngramRole   = if (length ngramParts > 1)
                    then ngramParts!!1
                    else ""
      year        = read (fields!!1) :: Int
      matchCount  = read (fields!!2) :: Int
  in if (year < aYear) || (ngramRole == "DET") -- Skip "determiners" (words like "a", "an", "the")
     then (wordCounts aYear restLines aMap)
     else (wordCounts aYear restLines
           (Map.insertWith (+) ngram matchCount aMap)
           -- aMap
           )

