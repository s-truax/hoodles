-- Original idea: just implement tokenizing, well-formedness, and decision
-- procedure for the PQ formal system. Evolves into discovering that the
-- natural first attempt of validate-then-parse can be simplified by just
-- parsing to determine well-formedness. aka, assume that your tokens are
-- well-formed and then try to parse them, raising an error if they are not
-- well formed. The error case means they are not well-formed.

import Data.List (group)

data PQSymbol = P | Q | H deriving (Eq, Show)

type PQString = [PQSymbol]

-- Data type representing derived theorems.
-- (Theorems whose derivation is known).
{-
Note: It seems like this type is a lot more natural / easy to work with when
using the bottom-up decision procedure. Probably because this type is
not representing possible theorems, only known theorems. It doesn't need
to represent arbitrary strings of the PQ system.

What led me to making this type was wanting to implement the iterative
decision procedure for the PQ system in addition to the inductive one. I was
thinking about how to solve the problem from an object-oriented mindset. I was
thinking "I would have some object representing a theorem that knew the
theorem that came before it." I was trying to mimic that here. It seems like
maybe I could have made the functions a bit more elegent, since Axiom is
almost a special case of theorem. At least this version is explicit.
-}
{-
Some more thoughts. This was prompted by wanting to make a function
Axiom -> Theorem that would ONLY take in Axioms, not theorems, and
apply the rule of production. I had some trouble squaring this, since
axioms ARE theorems. I think, if I actually did want to do this,
the solution would be to use the newtype keyword to make axiom and theorem
types. One use of the newtype keyword is to tell the type checker that you
want two things that are actually the same to be considered different things.

But even if I could make that function, it would operate on some representation
of a PQ theorem, which would probably most naturally be an (Int, Int, Int). The
problem here is that my abstract representation of a PQ string isn't isomorphic
to PQ strings. I can represent malformed PQ strings as an (Int, Int, Int), like
(-1, 0, 10). I also can't represent PQ strings like --q--p--qp--, but that isn't
as problematic, it just reinforces the fact that there isn't a clear
isomorphism.

So this seems to be a fundamental issue you run into with using types to model
your problem. My (Int, Int, Int) grammar can express things that don't make
sense in the grammer I'm trying to model. (Int, Int, Int) can represent PQ
expressions that don't make sense.

So this is where I'm thinking of "type safety". I get what it means now to
want to use types to make impossible situations inexpressible. Ideally, I should
be able to model a well-formed PQ string in a type where it is IMPOSSIBLE to
construct a malphormed PQ string. I think this would ideally be a 3-tuple of
nonzero natural numbers.

Takeaways here: your constructors are a good place to enforce invarients. Also,
you can sometimes lose totality when you represent one "grammar" with a
superset of that grammar. The example is that a three tuple of ints can
represent nonsensical pq strings.

In practice, this probably wouldn't matter. Just wrap everything in "Maybe."
But the purist approach would be, I think, to make it so that you can't
construct a malformed type.
-}

data PQTheorem = Axiom Int Int Int | Theorem Int Int Int (PQTheorem)
                 deriving (Eq, Show)

-- Infer a new theorem from an old one.
productionRule :: PQTheorem -> PQTheorem
productionRule a@(Axiom h1 h2 h3)        = Theorem h1 (h2 + 1) (h3 + 1) a
productionRule t@(Theorem h1 h2 h3 prev) = Theorem h1 (h2 + 1) (h3 + 1) t

-- Given a theorem, return the theorem that produced it using the
-- rule of production. If the theorem is an axiom, just return the axiom.
prevTheorem :: PQTheorem -> PQTheorem
prevTheorem a@(Axiom _ _ _)         = a
prevTheorem (Theorem h1 h2 h3 prev) = prev

theoremToString :: PQTheorem -> String
theoremToString thm = let
  hyphens        = repeat '-'
  buildStr x y z = take x hyphens ++ "p" ++ take y hyphens ++ "q" ++
    take z hyphens in
  case thm of (Axiom h1 h2 h3)     -> buildStr h1 h2 h3
              (Theorem h1 h2 h3 _) -> buildStr h1 h2 h3

printDerivation' :: PQTheorem -> String
printDerivation' a@(Axiom h1 _ _) = let
  hyphens = take h1 $ repeat '-' in
  "Let x = " ++ hyphens ++ ". Then use the axiom rule to form "
    ++ theoremToString a ++ ".\n"
printDerivation' t@(Theorem h1 h2 h3 prev) = let
  prevString = theoremToString prev
  currString = theoremToString t in
  printDerivation' prev ++ "Use the rule of production to form " ++
    currString ++ " from " ++ prevString ++ ".\n"

-- The (almost) purist's PQ Theorem / Derivation.
-- Works b/c we only have one axiom rule and one rule of production.
-- (Axiom k) is the (abs k)th axiom, and (ProduceFrom ds) is the rule of
-- production repeatedly applied to ds.
data Derivation = Axiom' Int | ProduceFrom Derivation (deriving Show)

-- Left fold?
countHyphens :: Derivation -> (Int, Int, Int)
countHyphens (Axiom' k)       = (1, abs k
countHyphens (ProduceFrom ds) = 1 + countHyphens ds

-- TODO: Make nicer.
interpret :: Derivation -> String
interpret (Axiom' k) = let
  hyphens = take (abs k) + 1 $ repeat '-'
  axiom   = hyphens ++ "p" ++ "-" ++ "q" ++ hyphens ++ "-" in
  "Let x = " ++ hyphens '-' ++ ". Then we may form the axiom xp-qx-, which" ++
  "is " ++ axiom
interpret (ProduceFrom ds) = let
  sofar   = interpret ds
  hyphens = countHyphens (ProduceFrom ds)
  thm     = 
  "Use the rule of production to produce "

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

-- TODO: Make the iterative version. Also think about if using this function
--       is cheating. (Like, am I relying ONLY on the rules or am I using
--       the fact that I know the rules are isomorphic to arithmetic?)
-- Given the representation of a PQ theorem, explain how to produce it.
decideInductively :: AbstractPQExpression -> Maybe [String]
decideInductively (x, y, z)
    | isMalformed = Nothing
    | isAxiom     = Just [axiomMsg]
    | otherwise = (++ [prodMsg]) <$> decideInductively (x, y-1, z-1)
    where isMalformed = x < 1 || y < 1 || z < 2
          isAxiom     = y == 1 && z == x + 1
          hyphens     = take x $ repeat '-'
          pqString    = exprToText (x, y, z)
          axiomMsg    = "Let x = " ++ hyphens ++ ". Then use the axiom rule " ++
                        "to form " ++ pqString
          prodMsg     = "Use the rule of production to form " ++ pqString

-- TODO: Find this in the standard libraries. Probably related to traversables.
--       It's sort of like we're transitioning states. Maybe related to
--       mapAccumL?
intersperceFn :: (a -> a -> a) -> a -> [a] -> [a]
intersperceFn f v []     = []
intersperceFn f v (x:xs) = v `f` x : intersperceFn f x xs

printDerivation :: [AbstractPQExpression] -> String
printDerivation [] = ""
printDerivation xs = let
  msgFunction x y = "Apply the rule of production to " ++ x ++ " to form " ++ y
  axiomMsg        = "Let x = " ++ hyphens ++ ". Then form the axiom " ++ axiom
  hyphens         = takeWhile (/='p') axiom
  axiom           = exprToText $ head xs in
  unlines $ (axiomMsg:) $ intersperceFn msgFunction axiom $ map exprToText $
    tail xs

main = do
  putStrLn "Enter a string of the PQ-system:"
  input <- getLine
  let truthValue = show $ decideString input
      message = "In the PQ-system, the truth value of this string is "
  putStrLn (message ++ truthValue)
