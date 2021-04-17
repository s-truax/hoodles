data Instruction = Z Int | S Int | T Int Int | J Int Int Int

newURM :: [Int] -> [Int]
newURM state = state ++ repeat 0

-- Replace the value in register 'reg' with 'f' applied to that value
modifyRegister :: [Int] -> (Int -> Int) -> Int -> [Int]
modifyRegister urm f 0   = f (head urm) : tail urm
modifyRegister urm f reg = head urm : modifyRegister (tail urm) f (reg - 1)

execZ :: [Int] -> Int -> [Int]
execZ urm = modifyRegister urm (\x -> 0)

execS :: [Int] -> Int -> [Int]
execS urm = modifyRegister urm (\x -> x + 1)

execT :: [Int] -> Int -> Int -> [Int]
execT urm m n = modifyRegister urm (\x -> urm !! m) n

execJ urm m n q curr =
  if (urm !! m) == (urm !! n)
    then q
      else curr + 1

runProgram' urm p currInstrNum =
  if currInstrNum >= length p
    then urm
         else
  case p !! currInstrNum of (Z n)     ->
                              runProgram' (execZ urm n) p (currInstrNum + 1)
                            (S n)     ->
                              runProgram' (execS urm n) p (currInstrNum + 1)
                            (T n m)   ->
                              runProgram' (execT urm n m) p (currInstrNum + 1)
                            (J n m q) ->
                              runProgram' urm p (execJ urm n m q currInstrNum)

runProgram urm p = head $ runProgram' urm p 0


-- Interactive tests
twothree = newURM [2, 3]
five = newURM [5]

addP = [(J 2 1 4), (S 0), (S 2), (J 0 0 0)] -- Computes x + y
sub1 = [(J 0 2 6), (S 2), (J 0 2 6), (S 1), (S 2), (J 0 0 2), (T 1 0)]

test1 = runProgram twothree addP -- should output 5
test2 = runProgram five sub1     -- expect 4
