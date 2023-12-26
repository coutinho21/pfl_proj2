-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023
import Data.List
import Data.Maybe
-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]
type Stack = [Integer]
type State = [(String, Integer)]

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map show (reverse stack))

state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ show val | (var, val) <- sortOn fst state]

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:code, stack, state) = case inst of
  Push n -> run (code, n : stack, state)
  Add -> binaryOp (+)
  Mult -> binaryOp (*)
  Sub -> binaryOp (-)
  Tru -> run (code, 1 : stack, state)
  Fals -> run (code, 0 : stack, state)
  Equ -> comparisonOp (==)
  Le -> comparisonOp (<=)
  And -> binaryOp (\x y -> if x /= 0 && y /= 0 then 1 else 0)
  Neg -> unaryOp (\x -> if x == 0 then 1 else 0)
  Fetch var -> run (code, val : stack, state) where val = fromMaybe (error "Run-time error") (lookup var state)
  Store var -> run (code, stack', state') where (val, stack') = pop stack; state' = (var, val) : state
  Noop -> (code, stack, state)
  Branch c1 c2 -> branchOp c1 c2
  Loop c1 c2 -> loopOp c1 c2
  where
    binaryOp op = case pop2 stack of
      (x, y, stack') -> run (code, op x y : stack', state)

    comparisonOp op = case pop2 stack of
      (x, y, stack') -> run (code, if op x y then 1 : stack' else 0 : stack', state)

    unaryOp op = case pop stack of
      (x, stack') -> run (code, op x : stack', state)

    branchOp c1 c2 = case pop stack of
      (x, stack') -> if x /= 0 then run (c1 ++ code, stack', state) else run (c2 ++ code, stack', state)

    loopOp c1 c2 = run (Branch (c1 ++ [Loop c1 c2, Noop]) [Noop] : code, stack, state)

    pop :: [a] -> (a, [a])
    pop [] = error "Run-time error"
    pop (x:xs) = (x, xs)

    pop2 :: [a] -> (a, a, [a])
    pop2 xs = case pop xs of
      (x, xs') -> case pop xs' of
        (y, xs'') -> (x, y, xs'')

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"









-- -- Part 2

-- -- TODO: Define the types Aexp, Bexp, Stm and Program

-- -- compA :: Aexp -> Code
-- compA = undefined -- TODO

-- -- compB :: Bexp -> Code
-- compB = undefined -- TODO

-- -- compile :: Program -> Code
-- compile = undefined -- TODO

-- -- parse :: String -> Program
-- parse = undefined -- TODO

-- -- To help you test your parser
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
--   where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- -- Examples:
-- -- testParser "x := 5; x := x - 1;" == ("","x=4")
-- -- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- -- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- -- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- -- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- -- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- -- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")