data Instruction =
    Z Integer
  | S Integer
  | T Integer Integer
  | J Integer Integer Integer deriving (Show)

-- Bijection N^2 -> N
tau m n = 2^m * (2 * n + 1) - 1

-- Inverse of tau
-- Haskell note: if I make the type signature of this function
-- `Int -> (Int, Int)`, I get a divide by zero error.
-- TODO: Optimize this.
aut r = let
  m  = length [ k | k <- [1..(r + 1)], ((r + 1) `mod` 2^k) == 0 ]
  n  = (((r + 1) `div` 2^m) - 1) `div` 2 in
  (m, n)

-- Bijection N^3 -> N
zeta m n q = tau (tau n m) q

-- Inverse of zeta
atez r = let
  (tauMN, q) = aut r
  (m, n)     = aut tauMN in
  (m, n, q)

beta :: Instruction -> Integer
beta (Z n)     = 4 * (n - 1)
beta (S n)     = 4 * (n - 1) + 1
beta (T m n)   = 4 * tau (m - 1) (n - 1) + 2
beta (J m n q) = 4 * zeta (m - 1) (n - 1) (q - 1) + 3
