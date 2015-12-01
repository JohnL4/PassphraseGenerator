-- | Functions for the passphrase generator.

{-# LANGUAGE BangPatterns #-}

module PassphraseGenerator where

import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)
import Text.Regex.TDFA

-- | The parts of speech (POS) we want to restrict our counting to.  We use "" to also pick up those
-- | ngrams that haven't been tagged with POS at all.
significantNgramRoles :: Set String -- Balanced binary tree; more efficient than [String], which is implemented as a linked list.
significantNgramRoles = fromList ["NOUN","VERB","ADJ","ADV"]

romanNumeral :: Regex
romanNumeral = makeRegexOpts defaultCompOpt{multiline=False} defaultExecOpt "^[ivxlc]+$"

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
  let fields          = splitOn "\t" aLine
      ngramParts      = splitOn "_" (fields!!0)
      ngram           = map toLower (ngramParts!!0) -- "ngram" is the same as "word", in this case.
      ngramRole       = if (length ngramParts > 1)
                        then ngramParts!!1
                        else ""
      year            = read (fields!!1) :: Int
      ngramMatchCount = read (fields!!2) :: Int
  in if (year < aYear)
        -- || (ngramRole == "DET") -- Skip "determiners" (words like "a", "an", "the")
        || ((length ngram) < 3)  -- Two-letter words
        || (not (member ngramRole significantNgramRoles))
        || (match romanNumeral ngram) -- Also need to eliminate Roman numerals: [ivxlc]+ (not using
                                      -- "m" because "mix" is a real word)
     then (wordCounts aYear restLines aMap)
     else (wordCounts aYear restLines
           (Map.insertWith (+) ngram ngramMatchCount aMap)
           -- aMap
           )

