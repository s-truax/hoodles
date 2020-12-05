-- Original idea: just implement tokenizing, well-formedness, and decision
-- procedure for the PQ formal system. Evolves into discovering that the
-- natural first attempt of validate-then-parse can be simplified by just
-- parsing to determine well-formedness. aka, assume that your tokens are
-- well-formed and then try to parse them, raising an error if they are not
-- well formed. The error case means they are not well-formed.

import Data.List (group)

data PQSymbol = P | Q | H deriving (Eq, Show)

type PQString = [PQSymbol]

type AbstractPQExpression = (Int, Int, Int)

-- Convert a String into a list of PQ tokens
tokenizePQ :: String -> Maybe PQString
tokenizePQ xs = case xs of []       -> Just []
                           ('p':xs) -> (:) <$> pure P <*> tokenizePQ xs
                           ('q':xs) -> (:) <$> pure Q <*> tokenizePQ xs
                           ('-':xs) -> (:) <$> pure H <*> tokenizePQ xs
                           (x:xs)   -> Nothing

-- Assert that a PQ-string is well formed.
-- Future Feature: tell WHY this string is not valid.
isValidPQ' :: PQString -> Bool
isValidPQ' xs = case group xs of
  [h1, [P], h2, [Q], h3] -> onlyHyphens (h1 ++ h2 ++ h3)
  otherwise              -> False

isValidPQ :: PQString -> Bool
isValidPQ xs = case parsePQ xs of Nothing  -> False
                                  (Just _) -> True

-- Assert that a PQ-string consists of only hyphens ('-')
onlyHyphens :: PQString -> Bool
onlyHyphens = all (== H)


-- Count the hyphens in a PQ string. Return nothing if the entire string
-- is not hyphens.
countHyphens :: PQString -> Maybe Int
countHyphens xs = case xs of []        -> Just 0
                             (H:ys)    -> (+1) <$> countHyphens ys
                             otherwise -> Nothing

-- Parse a PQ string.
-- Future feature: custom monad for parsing PQ strings.
parsePQ :: PQString -> Maybe AbstractPQExpression
parsePQ xs = do
  [h1, [P], h2, [Q], h3] <- return $ group xs
  a                      <- countHyphens h1
  b                      <- countHyphens h2
  c                      <- countHyphens h3
  return (a, b, c)

-- Assert that a PQString is a theorem.
-- Interestingly, in this system, it's easier to just check if something
-- is a theroem than to check if it's well formed or not.
-- The other option is to validate that the expression has the correct
-- form, then to check if it's a theorem. Maybe this should be called
-- isTrueStatement instead of isTheroem. isTheroem means it follows from the
-- axioms.
isTheroem :: PQString -> Bool
isTheroem xs = case parsePQ xs of Just (a, b, c) -> (a + b) == c
                                  Nothing        -> False

-- Determine if a string of the PQ system is a theorem of the PQ system.
decideString :: String -> Bool
decideString = maybe False isTheroem . tokenizePQ

exprToText :: AbstractPQExpression -> String
exprToText (x, y, z) = let
  ps = repeat 'p'
  qs = repeat 'q'
  hs = repeat '-' in
  take x hs ++ ['p'] ++ take y hs ++ ['q'] ++ take z hs

-- TODO: Make the iterative version.
-- Given the representation of a PQ theorem, explain how to produce it.
inductiveDecisionProcedure :: AbstractPQExpression -> Maybe [String]
inductiveDecisionProcedure (1, 1, 2) = Just ["Axiom rule to form -p-q--"]
inductiveDecisionProcedure (x, y, z) = let
  axiomRule = inductiveDecisionProcedure (x-1, y, z-1)
  prodRule  = inductiveDecisionProcedure (x, y-1, z-1)
  pqString  = exprToText (x, y, z)
  axiomMsg  = "Use the axiom rule to form " ++ pqString
  prodMsg   = "Use the rule of production to form " ++ pqString in
  if x < 0 || y < 0 || z < 0
    then Nothing
      else
        case (axiomRule, prodRule) of
          (Just xs, _) -> (++ [axiomMsg]) <$> axiomRule
          (_, Just xs) -> (++ [prodMsg]) <$> prodRule
          otherwise    -> Nothing

main = do
  putStrLn "Enter a string of the PQ-system:"
  input <- getLine
  let truthValue = show $ decideString input
      message = "In the PQ-system, the truth value of this string is "
  putStrLn (message ++ truthValue)
