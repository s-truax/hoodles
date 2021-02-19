-- Bijection N^2 -> N
tau :: Int -> Int -> Int
tau m n = 2^m * (2 * n + 1) - 1

-- Inverse of tau
-- Haskell note: if I make the type signature of this function
-- `Int -> (Int, Int)`, I get a divide by zero error.
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
