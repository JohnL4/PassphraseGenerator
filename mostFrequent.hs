-- | Reads stdin and prints the n most-frequent words occurring after year y to stdout, along with
-- | their counts.

{-# LANGUAGE BangPatterns #-}

import GHC.IO.Handle (hPutStr)
import GHC.IO.Handle.FD (stderr)
import Data.List (intersperse, sortBy)
import qualified Data.Map.Strict as Map

import PassphraseGenerator

{-
Test data:
let filetext = "aaa\t1900\t2\naaa\t1950\t3\nbbb\t1950\t5\nbbb_VERB\t1980\t9"
-}

-- | Number of most-frequent words we want.
n :: Int
n = 15000

-- | Earliest year we want to count while building frequency table
y :: Int
y = 1950

main :: IO()
main = do
{-
Read line, split on tabs, take first three fields, which are: 1-gram (word), year, match-count.
If year >= y:
   Trim parts of speech (POS) from 1-gram (leading, trailing known fragments delimited by "_")
      Known fragments: NOUN, VERB, ADJ, ADV, PRON (pronouns), DET (determiners and articles), ADP
      (prepositions, postpositions), NUM, CONJ, PRT (particles), X (miscellaneous)

   Split on "_", discard expected known fragments, complain if there are more than one fragments left.  (Note that I
   have verified that each ngram has 0 or 1 trailing attributes, so the check for an unexpected number of parts
   is unnecessary.)

   Find word in dictionary and add match-count to that entry.
At end of input, sort dictionary entries by match-counts (descending) and take first n entries for output.
-}
  allInput <- getContents
  putStrLn (concat (intersperse "\n" (map fst (take n (sortBy countDescending
                                                       (Map.toList (wordCounts y (lines allInput) Map.empty)))))))
  hPutStr stderr "Done.\n"
