-- From the Haskell wikibook chapter on HOFs.
-- The big questions were:
--   1. How do I sequence actions?
--      What does sequencing actions mean in an FP paradigm?
--   2. How do I coerce a type to an IO type? (use return).
for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i p f job =
    if p i
      then do 
          job i
          for (f i) p f job
        else
          return ()

main = do
    putStrLn "Enter a string. It will be repeated 10 times."
    word <- getLine
    for 0 (<10) (+1) (\x -> putStrLn word)


-- TODO: Describe this program in functional terms.
