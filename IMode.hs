import ParsePQ

-- Abstract representation of a PQ String.
-- Each argument of the tuple represents the number of left, right, and
-- middle hyphens, respectively.
type PQRepr = (Int, Int, Int)

-- Assert that a PQString is a theorem.
-- Interestingly, in this system, it's easier to just check if something
-- is a theroem than to check if it's well formed or not.
-- The other option is to validate that the expression has the correct
-- form, then to check if it's a theorem. Maybe this should be called
-- isTrueStatement instead of isTheroem. isTheroem means it follows from the
-- axioms.

-- UPDATE: This is kind of cheating. It's unclear that this tells us that
-- a given string is producable from axioms. It just tells us that the
-- arithmetic interpretation of a PQString is a true statement about
-- arithmetic
isTheroem :: PQString -> Bool
isTheroem xs = case parsePQ xs of Just (a, b, c) -> (a + b) == c
                                  Nothing        -> False

-- Determine if a string of the PQ system is a theorem of the PQ system.
-- UPDATE: Cheating for the same reason.
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
  let    message = "In the PQ-system, the truth value of this string is "
  putStrLn (message ++ truthValue)

