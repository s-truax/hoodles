data Instruction = Z Int | S Int | T Int Int | J Int Int Int

newURM :: [Int] -> [Int]
newURM state = state ++ repeat 0

changeRegister :: [Int] -> (Int -> Int) -> Int -> [Int]
changeRegister urm f 0   = f (head urm) : tail urm
changeRegister urm f reg = head urm : changeRegister (tail urm) f (reg - 1)

execZ :: [Int] -> Int -> [Int]
execZ urm = changeRegister urm (\x -> 0)

execS :: [Int] -> Int -> [Int]
execS urm = changeRegister urm (\x -> x + 1)

execT :: [Int] -> Int -> Int -> [Int]
execT urm m = changeRegister urm (\x -> urm !! m)

execJ urm m n q curr =
  if (urm !! m) == (urm !! n)
    then q
      else curr + 1

runProgram urm p = runProgram' urm p 0

runProgram' urm p currInstrNum =
  if currInstrNum >= length p
    then head urm
         else
  case p !! currInstrNum of (Z n)     ->
                              runProgram' (execZ urm n) p (currInstrNum + 1)
                            (S n)     ->
                              runProgram' (execS urm n) p (currInstrNum + 1)
                            (T n m)   ->
                              runProgram' (execT urm m n) p (currInstrNum + 1)
                            (J n m q) ->
                              runProgram' urm p (execJ urm n m q currInstrNum)


-- Interactive tests
twothree = newURM [2, 3]

addP = [(J 2 1 4), (S 0), (S 2), (J 0 0 0)] -- Computes x + y

test1 = runProgram twothree addP -- should output 5
