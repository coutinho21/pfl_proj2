-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023
import Data.List
import Data.Maybe
import Control.Exception
-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]
data StackVal = IntVal Integer | BoolVal Bool deriving Show
type Stack = [StackVal]
type State = [(String, StackVal)]

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map showVal stack)
  where
    showVal (IntVal n) = show n
    showVal (BoolVal b) = show b

state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ showVal val | (var, val) <- sortOn fst state]
  where
    showVal (IntVal n) = show n
    showVal (BoolVal b) = show b

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:code, stack, state) = case inst of
  Push n -> run (code, IntVal n : stack, state)
  Add -> binaryOp (\(IntVal x) (IntVal y) -> IntVal (x + y))
  Mult -> binaryOp (\(IntVal x) (IntVal y) -> IntVal (x * y))
  Sub -> binaryOp (\(IntVal x) (IntVal y) -> IntVal (x - y))
  Tru -> run (code, BoolVal True : stack, state)
  Fals -> run (code, BoolVal False : stack, state)
  Equ -> comparisonOp (\x y -> BoolVal (x == y))
  Le -> comparisonOp (\x y -> BoolVal (x <= y))
  And -> binaryOpBool (\x y -> BoolVal (x && y))
  Neg -> unaryOpBool (\(BoolVal x) -> BoolVal (not x))
  Fetch var -> run (code, val : stack, state) where val = fromMaybe (error "Run-time error") (lookup var state)
  Store var -> run (code, stack', state') where
    (val, stack') = pop stack  
    state' = (var, val) : filter ((/= var) . fst) state 
  Noop -> (code, stack, state)
  Branch c1 c2 -> branchOp c1 c2
  Loop c1 c2 -> loopOp c1 c2
  where
    binaryOp op = case pop2 stack of
      (x, y, stack') -> run (code, op x y : stack', state)

    binaryOpBool op = case pop2 stack of
      (BoolVal x, BoolVal y, stack') -> run (code, op x y : stack', state)
      _ -> error "Run-time error"

    comparisonOp op = case pop2 stack of
      (IntVal x, IntVal y, stack') -> run (code, op x y : stack', state)
      (BoolVal x, BoolVal y, stack') -> run (code, BoolVal (x == y) : stack', state)
      _ -> error "Run-time error"

    unaryOpBool op = case pop stack of
      (x, stack') -> run (code, op x : stack', state)

    branchOp c1 c2 = case pop stack of
      (BoolVal x, stack') -> if x then run (c1 ++ code, stack', state) else run (c2 ++ code, stack', state)
      _ -> error "Run-time error"

    loopOp c1 c2 = case run (c1, stack, state) of
      ([], stack', state') -> case pop stack' of
        (BoolVal x, stack'') -> if x then run (c2 ++ [Loop c1 c2], stack'', state') else run (code, stack'', state')
        _ -> error "Run-time error"
      (c1', stack', state') -> run (c1' ++ [Loop c1 c2], stack', state')

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




test :: (Code, (String, String)) -> IO String
test (code, expected) = 
  catch 
    (do
      let result = testAssembler code
      return $ if result == expected then "True" else "False")
    handler
  where
    handler :: SomeException -> IO String
    handler _ = return "Error"

testAll :: IO [String]
testAll = mapM test
  [ ([Push 10,Push 4,Push 3,Sub,Mult], ("-10",""))
  , ([Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"], ("","a=3,someVar=False,var=True"))
  , ([Fals,Store "var",Fetch "var"], ("False","var=False"))
  , ([Push (-20),Tru,Fals], ("False,True,-20",""))
  , ([Push (-20),Tru,Tru,Neg], ("False,True,-20",""))
  , ([Push (-20),Tru,Tru,Neg,Equ], ("False,-20",""))
  , ([Push (-20),Push (-21), Le], ("True",""))
  , ([Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"], ("","x=4"))
  , ([Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg][Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]], ("","fact=3628800,i=1"))
  ]


-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg][Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
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