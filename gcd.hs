gcd' :: Int -> Int -> Int
gcd' a b =
  let a' = max a b
      b' = min a b
      r = a' `mod` b'
  in if r == 0
    then b'
    else gcd' b' r
