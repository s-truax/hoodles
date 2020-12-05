-- Original idea: just implement tokenizing, well-formedness, and decision
-- procedure for the PQ formal system. Evolves into discovering that the
-- natural first attempt of validate-then-parse can be simplified by just
-- parsing to determine well-formedness. aka, assume that your tokens are
-- well-formed and then try to parse them, raising an error if they are not
-- well formed. The error case means they are not well-formed.

import Data.List (group)

data PQSymbol = P | Q | H deriving (Eq, Show)

type PQString = [PQSymbol]

-- Convert a String into a list of PQ tokens
tokenizePQ :: String -> Either String PQString
tokenizePQ xs = case xs of []       -> Right []
                           ('p':xs) -> (:) <$> pure P <*> tokenizePQ xs
                           ('q':xs) -> (:) <$> pure Q <*> tokenizePQ xs
                           ('-':xs) -> (:) <$> pure H <*> tokenizePQ xs
                           (x:xs)   -> Left ("Invalid token: " ++ [x])

-- Assert that a PQ-string is well formed.
-- Future Feature: tell WHY this string is not valid.
isValidPQ' :: PQString -> Bool
isValidPQ' xs = case group xs of
  [h1, [P], h2, [Q], h3] -> onlyHyphens (h1 ++ h2 ++ h3)
  otherwise              -> False

-- Assert that a PQ-string consists of only hyphens ('-')
onlyHyphens :: PQString -> Bool
onlyHyphens = all (== H)


-- Count the hyphens in a PQ string. Return nothing if the entire string
-- is not hyphens.
countHyphens :: PQString -> Maybe Int
countHyphens xs = case xs of []        -> Just 0
                             (H:ys)    -> (+) <$> 1 <*> countHyphens y
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

-- TODO: Assert that a PQString is a theorem.
-- Interestingly, in this system, it's easier to just check if something
-- is a theroem than to check if it's well formed or not.
isTheroem :: PQString -> Bool
isTheroem xs = case
