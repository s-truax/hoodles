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
aut :: Integer -> (Integer, Integer)
aut r = let
  m  = fromIntegral $ length [ k | k <- [1..(r + 1)], ((r + 1) `mod` 2^k) == 0 ]
  n  = (((r + 1) `div` 2^m) - 1) `div` 2 in
  (m, n)

-- Bijection N^3 -> N
zeta m n q = tau (tau n m) q

-- Inverse of zeta
atez :: Integer -> (Integer, Integer, Integer)
atez r = let
  (tauMN, q) = aut r
  (m, n)     = aut tauMN in
  (m, n, q)

beta :: Instruction -> Integer
beta (Z n)     = 4 * (n - 1)
beta (S n)     = 4 * (n - 1) + 1
beta (T m n)   = 4 * tau (m - 1) (n - 1) + 2
beta (J m n q) = 4 * zeta (m - 1) (n - 1) (q - 1) + 3

ateb :: Integer -> Instruction
ateb n = case n `mod` 4 of 0 -> Z $ n `div` 4 + 1
                           1 -> S $ (n - 1) `div` 4 + 1
                           2 -> uncurry T $ aut $ (n - 2) `div` 4 + 1
                           3 -> (\(x, y, z) -> (J x y z)) $
                                 atez $ (n - 3) `div` 4 + 1

-- TODO: The coding of [Instruction] -> Integer
-- Also, the code of the add program is going to be
-- 2^73727 + 2^{73727 + 1 + 1} + 2^{73727 + 1 + 9 + 2} +
-- + 2^{73727 + 1 + 9 + 2  + 8191 + 3}, which we'll never be able to print.
-- We're going to need some way to work with the codes of sequences.

{-
So now I should be able to write the function

        | 1 if phi_n(n) converges
f(n) =  |
        | 0 if phi_n(n) diverges or n >= 10

just by visual inspecetion of the first 10 functions.
-}
