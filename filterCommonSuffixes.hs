import Data.List

-- | Filter a word list, removing those words that are duplicates apart from common suffixes.  |
-- | Reads SORTED list on stdin, emits filtered list on stdout.
main :: IO()
main = do
  allInput <- getContents
  putStrLn ("Got " ++ (show (myLength (lines allInput))) ++ " lines")
  putStrLn ("Suffixes are:\n\t" ++ (concat (intersperse "\n\t" suffixes)))
  putStrLn ("suffixes is a list of length " ++ (show (myLength suffixes)))
  putStrLn ("Filtered list has " ++ (show (length (filterSuffixes (lines allInput)))) ++ " words.")
  putStrLn "Words:"
  putStrLn (concat (intersperse "\n" (filterSuffixes (lines allInput))))
  putStrLn "Done."

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
