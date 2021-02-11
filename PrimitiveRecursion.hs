data Natural = Z | S Natural deriving (Show)

add :: Natural -> Natural -> Natural
add x Z     = x
add x (S y) = S (add x y)

sub1 :: Natural -> Natural
sub1 Z     = Z
sub1 (S n) = n

sub :: Natural -> Natural -> Natural
sub x Z     = x
sub x (S y) = sub1 $ sub x y
