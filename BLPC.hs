{-# LANGUAGE FlexibleContexts #-}

{- Ben Lynn's Parser combinators -}

import Text.Parsec
import Data.Functor.Identity (Identity)

par ::
  Stream s Data.Functor.Identity.Identity t =>
  Parsec s () a -> s -> Either ParseError a
par parser source = parse parser "<GHCI>" source

{-
  number ::= [0-9]+
  symbol ::= '+' | '-' | '*' | '/'
  sexpr  ::= '(' <expr>* ')'
  expr   ::= <number> | <symbol> | <sexpr>
  lispy  ::= <expr>* EOF
-}

data LVal = Nil | Num Int | Sym Char | Cons LVal LVal deriving Show

parseLispy :: Parsec String () LVal
parseLispy = expr <* eof where
  num    = Num <$> read <$> many1 digit
  symbol = Sym <$> (char '+' <|> char '-' <|> char '*' <|> char '/')
  sexpr  = foldr Cons Nil <$> between (char '(') (char ')') (sepEndBy expr spaces)
  expr   = num <|> symbol <|> sexpr

{-
lookup :: Fractional a => Char -> Maybe (a -> a -> a)
lookup '+' = Just (foldr + 0)
lookup '-' = Just (foldr - 0)
lookup '*' = Just (*)
lookup '/' = Just (/)
lookup _   = Nothing

eval :: LVal -> LVal
eval Nil      = Nil
eval Num n    = n
eval Sym s    = s
eval Cons x y = eval x $ eval y
-}
