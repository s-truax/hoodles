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
isValidPQ :: PQString -> Bool
isValidPQ xs = case group xs of
  [h1, [P], h2, [Q], h3] -> onlyHyphens (h1 ++ h2 ++ h3)
  otherwise              -> False

-- Assert that a PQ-string consists of only hyphens ('-')
onlyHyphens :: PQString -> Bool
onlyHyphens = all (== H)

-- Parse a list of PQ
parsePQ :: PQString -> (Int, Int, Int)

-- TODO: Assert that a PQString is a theorem.
-- Interestingly, in this system, it's easier to just check if something
-- is a theroem than to check if it's well formed or not.
isTheroem :: PQString -> Bool
isTheroem xs = case
