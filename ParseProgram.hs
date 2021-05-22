{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Text.Read (readMaybe)
import Control.Monad.Identity (Identity)
import System.Environment (getArgs)
import System.IO (readFile)

import URM (Instruction(..)
           , URM
           , newURM
           , runProgram
           , runProgramVerbose2
           )

{-
  <num>         ::= [0-9]+
  <instruction> ::= J(<num>, <num>, <num>) | T(<num>, <num>)
                    | Z(<num>, <num>) | S(<num>, <num>) ;
  <program>     ::= <instruction>*
-}

paddedSep :: Parsec s u a -> Parsec s u b -> Parsec s u a
paddedSep p padding = do
  padding
  x <- p
  padding
  return x

readInt :: Parsec String () Int
readInt = do
  x <- many1 digit
  case readMaybe x of
    Just k -> return k
    Nothing -> fail "bad integer" <?> "digit"

-- Read n Int arguments separated by commas and surrounded by an arbitrary
-- number of spaces, e.g, "1,  2,    3     , 4"
readArgs' :: Int -> Parsec String () [Int]
readArgs' 0 = spaces >> return []
readArgs' 1 = do
  spaces
  x <- readInt
  spaces
  return [x]
readArgs' n = do
  spaces
  x  <- readInt
  spaces
  char ','
  xs <- readArgs' (n - 1)
  return (x:xs)

-- Read arguments between parenthesis
readArgs :: Int -> Parsec String () [Int]
readArgs n = between (char '(') (char ')') (readArgs' n)

parseJ :: Parsec String () Instruction
parseJ = do
  char 'J'
  [m, n, q] <- readArgs 3
  return (J m n q)

parseT :: Parsec String () Instruction
parseT = do
  char 'T'
  [m, n] <- readArgs 2
  return (T m n)

parseZ :: Parsec String () Instruction
parseZ = do
  char 'Z'
  [n] <- readArgs 1
  return (Z n)

parseS :: Parsec String () Instruction
parseS = do
  char 'S'
  [n] <- readArgs 1
  return (S n)

parseProgram :: Parsec String () [Instruction]
parseProgram = many line <* eof where
  line = choice [parseJ, parseT, parseS, parseZ] <* newline

-- Pretty-print output of runProgramVerbose2
-- TODO: The Nothing case feels like it should be impossible.
pprintStates :: Maybe [(URM, URM, Instruction)] -> IO ()
pprintStates Nothing   = print "Something terrible has happened"
pprintStates (Just xs) = mapM_ print xs

main :: IO ()
main = do
  args     <- getArgs
  let fname       = head args
  let programArgs = sequenceA $ readMaybe <$> tail args
  let urm         = newURM <$> programArgs
  contents <- readFile fname
  case parse parseProgram fname contents of
                                -- Maybe [(URM, URM, Instruction)]
    Right p       -> pprintStates (runProgramVerbose2 <$> pure p <*> urm)
    Left parseErr -> (print parseErr)

{- TODO:
   * Add some failure messages to main
-}
