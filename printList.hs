-- Can we write this with a fold?
printList :: (Show a) => [a] -> IO ()
printList []         = do return ()
printList (x:xs)     = do
    print x
    printList xs

-- At first I thought it was an issue that the base cases
-- return the empty list. But I guess this is OK, because
-- the return type of this function is a list of something,
-- and it leaves the possibility that the list is empty.
-- Can we write this as a fold?
splice :: [a] -> [b] -> [(a, b)]
splice xs []         = []
splice [] ys         = []
splice (x:xs) (y:ys) = (x, y) : splice xs ys

enumerate :: [a] -> [(Integer, a)]
enumerate = splice [1..]

removeListDelims :: String -> String
removeListDelims = let
    listDelims = ['[', ']', ',', ' ']
    in filter (\x -> not $ elem x listDelims)

