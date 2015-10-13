{-
Playing around with records.
-}

main :: IO()
main = do
  dumpThings [Thing { name = "Alice", size = 12}, Thing { name = "Bob", size=33}]
  putStrLn "Done."
  
data Thing = Thing { name :: String,
                     size :: Int
                   }
              deriving( Show)

dumpThings :: (Show a) => [a] -> IO()
dumpThings [things] = do
  putStrLn "Things:"
  actions <- map putStrLn things
  putStrLn $ (show (length actions)) ++ " actions."

