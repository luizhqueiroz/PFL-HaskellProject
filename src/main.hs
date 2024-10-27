-- PFL 2023/24 - Haskell practical assignment
-- Part 1

import qualified Stack
import Stack (Stack)
import qualified State
import State (State)
import Value

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show -- Representação das possíveis instruções 
type Code = [Inst] -- Representação de uma lista de instruções

createEmptyStack :: Stack -- Cria uma stack vazia
createEmptyStack = Stack.empty

stack2Str :: Stack -> String -- Recebe uma stack e devolve sua representação em formato string
stack2Str stk = show stk

createEmptyState :: State -- Cria um state vazio
createEmptyState = State.empty

state2Str :: State -> String -- Recebe um state e devolve a sua representação em formato string
state2Str stt = show (State.stateSort stt)

run :: (Code, Stack, State) -> (Code, Stack, State) -- Recebe a lista de instruções (code), stack e state, realiza as instruções presentes na lista e devolve a lista de instruções vazia e a stack e o state resultante
run ([], stk, stt) = ([], stk, stt)
run (((Push n):xs), stk, stt) = run (xs, Stack.push (ValorInt n) stk, stt)
run (((Add):xs), stk, stt) = run (xs, Stack.calculate "Add" stk, stt)
run (((Mult):xs), stk, stt) = run (xs, Stack.calculate "Mult" stk, stt)
run (((Sub):xs), stk, stt) = run (xs, Stack.calculate "Sub" stk, stt)
run (((Tru):xs), stk, stt) = run (xs, Stack.push (ValorBool True) stk, stt)
run (((Fals):xs), stk, stt) = run (xs, Stack.push (ValorBool False) stk, stt)
run (((Equ):xs), stk, stt) = run (xs, Stack.compare "Equ" stk, stt)
run (((Le):xs), stk, stt) = run (xs, Stack.compare "Le" stk, stt)
run (((And):xs), stk, stt) = run (xs, Stack.compare "And" stk, stt)
run (((Neg):xs), stk, stt) = run (xs, Stack.neg stk, stt)
run (((Fetch s):xs), stk, stt) = run (xs, Stack.push (State.fetch s stt) stk, stt)
run (((Store s):xs), stk, stt) = run (xs, Stack.pop stk, State.store (s, (Stack.top stk)) stt)
run (((Noop):xs), stk, stt) = run (xs, stk, stt)
run (((Branch code1 code2):xs), stk, stt)
  | (Stack.top stk) == (ValorBool True) = run (code1 ++ xs, Stack.pop stk, stt)
  | (Stack.top stk) == (ValorBool False) = run (code2 ++ xs, Stack.pop stk, stt)
  | otherwise = error "Run-time error"
run (((Loop code1 code2):xs), stk, stt) = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ xs, stk, stt)

-- To help you test your assembler
testAssembler :: Code -> (String, String) -- Recebe a lista de instruções e devolve a stack e o state no formato de teste para validação das funções implementadas
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

-- Part 2

data Aexp = Val Integer | Var String | Adds Aexp Aexp | Subs Aexp Aexp | Mults Aexp Aexp deriving Show -- Representação das possíveis expressões aritmética

data Bexp = BoolVal Bool | Negs Bexp | EqA Aexp Aexp | EqB Bexp Bexp | Les Aexp Aexp | Ands Bexp Bexp deriving Show -- Representação das possíveis expressões booleanas

data Stm = Assign String Aexp | Seq Stm Stm | IfThenElse Bexp Stm Stm | WhileLoops Bexp Stm deriving Show -- Representação dos possíveis statements

type Program = [Stm] -- Representação de uma lista de statements

compA :: Aexp -> Code -- Recebe uma expressão aritmética e transforma numa lista de instruções que realiza essa expressão
compA (Val n) = [Push n]
compA (Var x) = [Fetch x]
compA (Adds a b) =  compA b ++ compA a ++ [Add]
compA (Subs a b) =  compA b ++ compA a ++ [Sub]
compA (Mults a b) = compA b ++ compA a ++ [Mult]

compB :: Bexp -> Code -- Recebe uma expressão booleana e transforma numa lista de instruções que realiza essa expressão
compB (BoolVal True) = [Tru]
compB (BoolVal False) = [Fals]
compB (Negs n) = compB n ++ [Neg]
compB (EqA a b) = compA b ++ compA a ++ [Equ]
compB (EqB a b) = compB b ++ compB a ++ [Equ]
compB (Les a b) = compA b ++ compA a ++ [Le]
compB (Ands a b) = compB b ++ compB a ++ [And]

compile :: Program -> Code -- Recebe uma lista de statements e transforma os statements numa lista de instruções que realize cada statement
compile [] = []
compile ((Assign s aexp):xs) = compA aexp ++ [Store s] ++ compile xs
compile ((Seq s1 s2):xs) = compile [s1, s2] ++ compile xs
compile ((IfThenElse bexp s1 s2):xs) = compB bexp ++ [Branch (compile [s1]) (compile [s2])]  ++ compile xs
compile ((WhileLoops bexp stm):xs) = [Loop (compB bexp) (compile [stm])] ++ compile xs

parse :: String -> Program
parse s = buildData (lexer s) -- Recebe a string e devolve o parse dessa string como uma lista de statements

lexer :: String -> [String] -- Recebe uma string, divide a string numa lista de palavras e devolve essa lista
lexer s = foldr checkChar [""] s

checkChar :: Char -> [String] -> [String] -- Recebe um character e uma lista de string, verifica se o character é um espaço, adiciona uma string vazia a lista se for, caso contrario adiciona o caracter no inicio da primeira string da lista e devolve a nova lista
checkChar c acc@(current:rest)
    | c == ' '  = "" : acc
    | otherwise = (c:current) : rest

buildData :: [String] -> Program -- Recebe uma lista de palavras referente ao código do programa e devolve uma lista de statements construída a partir da lista recebida
buildData (x:xs) -- TODO
  | x == "x" = [Assign "x" (Val 2)]
  | otherwise = error "incomplete"

-- To help you test your parser
testParser :: String -> (String, String) -- Recebe a string com o código do programa e devolve a stack e o state no formato de teste para validação da implementação
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
