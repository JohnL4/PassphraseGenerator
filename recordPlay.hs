{-
Playing around with records.
-}

import Data.List (intersperse)

main :: IO()
main = do
  dumpThings [Thing { name = "Alice", size = 12}, Thing { name = "Bob", size=33}]
  putStrLn "Done."
  
data Thing = Thing { name :: String,
                     size :: Int
                   }
              deriving( Show)

dumpThings :: (Show a) => [a] -> IO()
dumpThings things = do
  putStrLn ("Things:\n\t" ++ (concat (intersperse "\n\t" (map show things))))

