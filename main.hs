-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023
import Data.List
import Data.Maybe
import Control.Exception
import Text.Read (readMaybe)
import Data.List (intercalate)


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



-- -- Part 2

-- Arithmetic expressions
data Aexp = Var String
          | IntConst Integer
          | Add2 Aexp Aexp
          | Sub2 Aexp Aexp
          | Mul Aexp Aexp
          | BinaryOp String Aexp Aexp
          deriving Show

-- Boolean expressions
data Bexp = BTrue
          | BFalse
          | Eq Aexp Aexp
          | Leq Aexp Aexp
          | Not Bexp
          | And2 Bexp Bexp
          deriving Show

-- Statements
data Stm = Assign String Aexp
         | Seq [Stm]
         | If Bexp Stm Stm
         | While Bexp Stm
         | Compound Stm Stm
         deriving Show

-- Program
type Program = [Stm]


compA :: Aexp -> Code
compA (Var x) = [Fetch x]
compA (IntConst n) = [Push n]
compA (Add2 a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (Sub2 a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (Mul a1 a2) = compA a1 ++ compA a2 ++ [Mult]
compA (BinaryOp op a1 a2) = case op of
  "-" -> compA a2 ++ compA a1 ++ [Sub]
  _ -> compA a1 ++ compA a2 ++ case op of
    "+" -> [Add]
    "*" -> [Mult]
  
compB :: Bexp -> Code
compB BTrue = [Push 1]
compB BFalse = [Push 0]
compB (Eq a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (Leq a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (Not b) = compB b ++ [Neg]
compB (And2 b1 b2) = compB b1 ++ compB b2 ++ [And]

compile :: Program -> Code
compile [] = []
compile ((Assign x a):ss) = compA a ++ [Store x] ++ compile ss
compile ((Seq ss):ss') = compile ss ++ compile ss'
compile ((If b s1 s2):ss) = 
    let codeB = compB b
        codeS1 = compile [s1]
        codeS2 = compile [s2]
    in codeB ++ [Branch codeS1 codeS2] ++ compile ss
compile ((While b s):ss) = [Loop (compB b) (compile [s])] ++ compile ss
compile ((Compound s1 s2):ss) = compile [s1] ++ compile [s2] ++ compile ss


type Token = String


tokenize :: String -> [Token]
tokenize = words . concatMap spaceOut . replaceAll
  where
    spaceOut c = if c `elem` "();+-*/"
                  then [' ', c, ' ']
                  else [c]
    replaceAll = replace ":=" ":= " . replace "<=" "<= " . replace ">=" ">= " . replace "==" "== "
    replace old new = intercalate new . words

parseAexp :: [Token] -> (Aexp, [Token])
parseAexp (t:ts) 
  | Just n <- readMaybe t = (IntConst n, ts)
  | otherwise = case ts of
      (op:a2:ts') | op `elem` ["+", "-", "*", "/"] -> 
        let (a2', ts'') = parseAexp (a2:ts')
        in (BinaryOp op (Var t) a2', ts'')
      _ -> (Var t, ts)

parseBexp :: [Token] -> (Bexp, [Token])
parseBexp ("not":ts) = let (b, ts') = parseBexp ts in (Not b, ts')
parseBexp ("(":ts) = 
  let (b, ")":ts') = parseBexp ts
  in (b, ts')
parseBexp ts = 
  let (a1, op:ts') = parseAexp ts
      (a2, ts'') = parseAexp ts'
  in case op of
       "==" -> (Eq a1 a2, ts'')
       "<=" -> (Leq a1 a2, ts'')
       _ -> error $ "Unknown operator: " ++ op
            

parseStm :: [Token] -> (Stm, [Token])
parseStm [] = error "Unexpected end of input"
parseStm ("(":ts) = 
  let (s, ts') = parseStm ts
  in case ts' of
       (";":ts'') -> let (s', ";":ts''') = parseStm ts''
                     in case ts''' of
                          (")":ts'''') -> ((Compound s s'), ts'''')
                          _ -> error $ "Expected ')', got: " ++ show ts'''
       _ -> error $ "Expected ';', got: " ++ show ts'

parseStm (t:":=":ts) = 
  let (a, ts') = parseAexp ts
  in (Assign t a, ts')
parseStm ("if":ts) = 
  let (b, "then":ts') = parseBexp ts
      (s1, ";":ts'') = parseStm ts'
  in case ts'' of
       ("else":ts''') -> let (s2, ts'''') = parseStm ts'''
                         in (If b s1 s2, ts'''')
       _ -> error $ "Expected 'else', got: " ++ show ts''
parseStm ("while":ts) = 
  let (b, ts') = parseBexp ts
  in case dropWhile (/= "do") ts' of
       ("do":ts'') -> let (s, ts''') = parseStm ts''
                      in (While b s, ts''')
       _ -> error $ "Expected 'do', got: " ++ show ts'
parseStm ts = error $ "Unexpected tokens: " ++ show ts

parseProgram :: [Token] -> Program
parseProgram [] = []
parseProgram ts = 
  let (s, ts') = parseStm ts
      ts'' = case ts' of
        (";":rest) -> rest
        _ -> ts'
  in s : parseProgram ts''

parse :: String -> Program
parse = parseProgram . tokenize

-- -- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
   where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

test2 :: (String, (String, String)) -> IO String
test2 (code, expected) = 
  catch 
    (do
      let result = testParser code
      return $ if result == expected then "True" else "False")
    handler
  where
    handler :: SomeException -> IO String
    handler _ = return "Error"

testAll2 :: IO [String]
testAll2 = mapM test2
  [ ("x := 5; x := x - 1;", ("","x=4"))
  , ("i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);", ("","fact=3628800,i=1"))
  , ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;", ("","x=2,z=4")) --da mal porque faz o else mesmo fazendo o if
  , ("x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)", ("","x=1"))  --da mal porque faz o else mesmo fazendo o if
  , ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;", ("","x=2"))    --da mal porque faz o else mesmo fazendo o if
  , ("if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;", ("","y=2")) --da erro
  , ("x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);", ("","x=2,y=-10,z=6"))   --da erro
  ]