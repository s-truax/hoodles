import Data.List (maximumBy)
-- Derive functor? Bifunctor?
data Instruction = Z Int | S Int | T Int Int | J Int Int Int deriving Show

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
                            (T m n)   ->
                              runProgram' (execT urm m n) p (currInstrNum + 1)
                            (J m n q) ->
                              runProgram' urm p (execJ urm m n q currInstrNum)

runProgram urm p = head $ runProgram' urm p 0

-- Run the URM in verbose mode. K is the number of registers to print.
runProgramVerbose' k currInstrNum p urm = 
  if currInstrNum >= length p then [] else
    case p !! currInstrNum of
      (Z n)     -> [(take k urm, take k $ execZ urm n, (Z n))] ++
                     runProgramVerbose' k (currInstrNum + 1) p (execZ urm n)
      (S n)     -> [(take k urm, take k $ execS urm n, (S n))] ++
                     runProgramVerbose' k (currInstrNum + 1) p (execS urm n)
      (T m n)   -> [(take k urm, take k $ execT urm m n, (T m n))] ++
                     runProgramVerbose' k (currInstrNum + 1) p (execT urm m n)
      (J m n q) -> [(take k urm, take k urm, (J m n q))] ++
                     runProgramVerbose' k (execJ urm m n q currInstrNum) p urm

-- Print 7 registers by default. 
runProgramVerbose = runProgramVerbose' 7 0

-- Utility functions

-- Make all jump instructions J m n q jump to J m n (q + newStart)
pushJumps :: Int -> [Instruction] -> [Instruction]
pushJumps newStart = let
  editor (J m n q) = (J m n (newStart + q))
  editor instr     = instr in
  map editor

-- Put a URM program in standard form
standardize :: [Instruction] -> [Instruction]
standardize program = let
  s                = length program
  editor instr@(J m n q) = if q > s then (J m n s) else instr
  editor instr           = instr in
  map editor program

-- Standardizes then concatenates two programs
concatPrograms :: [Instruction] -> [Instruction] -> [Instruction]
concatPrograms p q = standardize p ++ pushJumps (length p) (standardize q)

-- return the maximum register referenced by an instruction
maxVal :: Instruction -> Int
maxVal (S n)     = n
maxVal (Z n)     = n
maxVal (T m n)   = max m n
maxVal (J m n q) = maximum [ m, n, q ]

-- Smallest number larger than all registers mentioned in the program.
rho :: [Instruction] -> Int
rho = (+1) . maximum . map maxVal

-- projection i returns the functon that takes the ith argument of n
-- input arguments
projection :: Int -> [Instruction]
projection i = [ T i 0]

-- Takes a list of register locations and a program. Returns a new
-- program that computes the given program on the inputs in the given
-- registers, then moves the result to the desired register
moveComputeMove :: [Int] -> Int -> [Instruction] -> [Instruction]
moveComputeMove starts end p = let
  transfers = [ T m n | (m, n) <- zip starts [0, 1..] ]
  store     = [ T 0 end ] in
  transfers `concatPrograms` p `concatPrograms` store

-- compose the program that computes f with the programs that compute g_i.
-- we need to specify the arity of f and the gs.
-- Also... am I abusing `let`? How do I not make my function definitions (h)uge?
-- Debug:
-- TODO: Break down these let bindings and test them individually.
compose :: Int -> Int -> [Instruction] -> [[Instruction]] -> [Instruction]
compose fArity gsArity f gs = let
  n        = gsArity
  k        = fArity
  gsMaxRho = maximum $ map rho gs
  m        = maximum [ fArity, gsArity, rho f, gsMaxRho ]
  pushArgs = [ T i (m + i) | i <- [0..(n - 1)] ]
  runGs    = [ moveComputeMove [m..(m + n - 1)] (m + n + i) g | (i, g) <- zip [0..(k - 1)] gs ]
  concatGs = foldr concatPrograms [] runGs
  runF     = moveComputeMove [(m + n)..(m + n + k - 1)] 0 f
  in pushArgs `concatPrograms` concatGs `concatPrograms` runF
  
  

-- Debug functions
pprintStates :: [[Int]] -> IO ()
pprintStates = putStrLn . unlines . map show

-- Interactive tests
twothree = newURM [2, 3]
five = newURM [5]

addP = [(J 2 1 4), (S 0), (S 2), (J 0 0 0)] -- Computes x + y
sub1 = [(J 0 2 6), (S 2), (J 0 2 6), (S 1), (S 2), (J 0 0 2), (T 1 0)]
sub  = []  -- TODO

test1 = runProgram twothree addP -- should output 5
test2 = runProgram five sub1     -- expect 4
