-- | Filter a word list, removing those words that are duplicates apart from common suffixes.  |
-- | Reads SORTED list on stdin, emits filtered list on stdout.
main :: IO()
main = do
  allInput <- getContents
  putStrLn ("Got " ++ (show (myLength (lines allInput))) ++ " lines")
  putStrLn (show suffixes)
  putStrLn ("suffixes is a list of length " ++ (show (myLength suffixes)))
  putStrLn "Done."

-- | The common suffixes we will be removing
suffixes :: [[Char]]
suffixes = ["s","es","ed","ing","ly"] 

myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)
