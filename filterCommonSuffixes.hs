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

import GHC.IO.Handle (hPutStr)
import GHC.IO.Handle.FD (stderr)

-- | Filter a word list, removing those words that are duplicates apart from common suffixes.
-- | Reads SORTED list on stdin, emits filtered list on stdout.
main :: IO()
main = do
  allInput <- getContents
  hPutStr stderr ("Got " ++ (show (length (lines allInput))) ++ " lines\n")
  hPutStr stderr ("Filtered list has " ++ (show (length (filterSuffixes (lines allInput)))) ++ " words.\n")
{-
  hPutStr stderr "Filtered words:\n"
  putStrLn (concat (intersperse "\n" (filterSuffixes (lines allInput))))
-}
  hPutStr stderr "Done.\n"

-- | The common suffixes we will be removing
suffixes :: [[Char]]
suffixes = ["s","es","ed","ing","ly"] 

-- | The actual filter function. Removes words that are like prefix apart from common suffix.
filterSuffixes :: [[Char]] -> [[Char]]
filterSuffixes [] = []
filterSuffixes [lastword] = [lastword]
filterSuffixes (w : ws) = (filterSuffixesInCluster (cluster (w:ws)))
                       ++ (filterSuffixes (drop (length (cluster (w:ws))) (w:ws)))

-- | Returns a cluster of words sharing a common prefix from the next few words of the given list.
cluster :: [[Char]] -> [[Char]]
cluster [] = []
cluster [w] = [w]
cluster (w:ws)                  -- First word will be shortest, since (a) the input list is sorted,
                                -- and (b) previous cluster would have broken off when prefix changed.
  = w : clusterSharingPrefix w ws

-- | Returns all words from the given list having the given word as a prefix.
clusterSharingPrefix :: [Char] -> [[Char]] -> [[Char]]
clusterSharingPrefix w [] = []
clusterSharingPrefix w ws
  | w == (take (length w) (head ws))   = (head ws) : clusterSharingPrefix w (tail ws)
  | otherwise                          = []

-- | Filters words from given cluster known to have a common prefix having a known transformation
-- | from one of the words in the cluster.
filterSuffixesInCluster :: [[Char]] -> [[Char]]
filterSuffixesInCluster [] = []
filterSuffixesInCluster [word] = [word]
filterSuffixesInCluster ws
  -- Need to build a dictionary? No, suffixes should already be a dictionary.
  -- 
  -- Need to find, for each transformation rule in the dictionary, a match in the cluster, and
  -- eliminate the transformed word of the pair.
  = let tps = transformationPairs ws
    in ws

-- | Returns recognized pairs of words from the given word cluster, in which each pair consists of
-- | two words from the cluster, the first of which can be transformed to the second of which via
-- | one of the transformation rules defined in suffixes.
transformationPairs :: [[Char]] -> [([Char],[Char])]
transformationPairs ws = []


-- | Returns true if the given word appends one of the common suffixes to the given prefix.
hasSuffix :: [Char] -> [Char] -> Bool
hasSuffix prefix word 
  | isPrefix prefix word   = elem (drop (length prefix) word) suffixes
  | otherwise              = False

-- | Returns true if the given prefix is actually a prefix of the given word
isPrefix :: [Char] -> [Char] -> Bool
isPrefix prefix word = (take (length prefix) word) == prefix
