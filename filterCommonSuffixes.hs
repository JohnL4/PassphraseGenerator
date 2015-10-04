{-
So, what we REALLY want to do is: recognize clumps of prefixed words, and within said clump, remove
those words that only add a standard suffix to the base word.

So, for example, in the following list:

   access
   accessible
   accessories
   accessorize
   accessory

We would recognize "accessory" as the root word (somehow), while ignoring "access" and "accessible"
as not part of the "accessory" clump.  The idea is that we would remove only "accessories" from
this run of words, being the plural (y --> ies) of "accessory".

So, we have to build up clumps of words, then scan clump for roots and remove those words that
match transformations from the root word.  Repeatedly, I guess.  

-}

import Data.List
import GHC.IO.Handle (hPutStr)
import GHC.IO.Handle.FD (stderr)

-- | Filter a word list, removing those words that are duplicates apart from common suffixes.
-- | Reads SORTED list on stdin, emits filtered list on stdout.
main :: IO()
main = do
  allInput <- getContents
  hPutStr stderr ("Got " ++ (show (myLength (lines allInput))) ++ " lines\n")
  hPutStr stderr ("Filtered list has " ++ (show (length (filterSuffixes (lines allInput)))) ++ " words.\n")
  hPutStr stderr "Filtered words:\n"
  putStrLn (concat (intersperse "\n" (filterSuffixes (lines allInput))))
  hPutStr stderr "Done.\n"

-- | The common suffixes we will be removing
suffixes :: [[Char]]
suffixes = ["s","es","ed","ing","ly"] 

-- | Just a stupid length function to show that we can use pure functions on something bound with
-- | the "<-" operator.
myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)

-- | The actual filter function. Removes words that are like prefix apart from common suffix.
filterSuffixes :: [[Char]] -> [[Char]]
filterSuffixes [] = []
filterSuffixes [lastword] = [lastword]
filterSuffixes (prefix : [lastword])
  | (hasSuffix prefix lastword)  = [prefix]
  | otherwise                    = prefix : [lastword]
filterSuffixes (prefix : nextword : restwords)
  | (hasSuffix prefix nextword)  = filterSuffixes (prefix : restwords)
  | otherwise                    = prefix : (filterSuffixes (nextword : restwords))

-- | Returns true if the given word appends one of the common suffixes to the given prefix.
hasSuffix :: [Char] -> [Char] -> Bool
hasSuffix prefix word 
  | isPrefix prefix word   = elem (drop (length prefix) word) suffixes
  | otherwise              = False

-- | Returns true if the given prefix is actually a prefix of the given word
isPrefix :: [Char] -> [Char] -> Bool
isPrefix prefix word = (take (length prefix) word) == prefix
