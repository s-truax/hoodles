sequenceIO :: [IO a] -> IO [a]
sequenceIO xs = let
    sequenceIOhelper acc []     = do
              return acc
    sequenceIOhelper acc (x:xs) = do
        xVal <- x
        sequenceIOhelper (xVal:acc) xs
    in
        do
            sequenceIOhelper [] xs

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f = sequenceIO . map f  -- Pointfree?

