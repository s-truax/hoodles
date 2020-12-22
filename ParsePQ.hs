module ParsePQ where

import Data.List (group)

data PQSymbol = P | Q | H deriving (Eq, Show)

type PQString = [PQSymbol]

-- Convert a String into a list of PQ tokens
tokenizePQ :: String -> Maybe PQString
tokenizePQ xs = case xs of []       -> Just []
                           ('p':xs) -> (:) <$> pure P <*> tokenizePQ xs
                           ('q':xs) -> (:) <$> pure Q <*> tokenizePQ xs
                           ('-':xs) -> (:) <$> pure H <*> tokenizePQ xs
                           (x:xs)   -> Nothing

-- Count the hyphens in a PQ string. Return nothing if the entire string
-- is not hyphens.
countHyphens :: PQString -> Maybe Int
countHyphens xs = case xs of []        -> Just 0
                             (H:ys)    -> (+1) <$> countHyphens ys
                             otherwise -> Nothing

-- Parse a PQ string.
-- Future feature: custom monad for parsing PQ strings.
parsePQ :: PQString -> Maybe (Int, Int, Int)
parsePQ xs = do
  [h1, [P], h2, [Q], h3] <- return $ group xs
  a                      <- countHyphens h1
  b                      <- countHyphens h2
  c                      <- countHyphens h3
  return (a, b, c)

isValidPQ :: PQString -> Bool
isValidPQ xs = case parsePQ xs of Nothing  -> False
                                  (Just _) -> True

-- I kind of like the pointful definition better than the point-free one.
parseFromText :: String -> Maybe (Int, Int, Int)
parseFromText = (=<<) parsePQ . tokenizePQ

