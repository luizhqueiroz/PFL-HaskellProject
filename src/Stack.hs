module Stack (Stack, push, pop, top, calculate, Stack.compare, neg, empty, isEmpty) where
    import Value

    data Stack = Stk [Value] -- Representação da stack

    push :: Value -> Stack -> Stack -- Adiciona um elemento do tipo Value no topo da stack e devolve a nova stack
    push x (Stk xs) = Stk (x:xs)

    pop :: Stack -> Stack -- Retira o elemento do topo da stack e devolve a nova stack
    pop (Stk (_:xs)) = Stk xs
    pop _ = error "Run-time error"

    top :: Stack -> Value -- Devolve o elemento do tipo Value no topo da stack
    top (Stk (x:_)) = x
    top _ = error "Run-time error"

    calculate:: String -> Stack -> Stack -- Recebe a string dizendo o tipo de operação artimética que deseja realizar, recebe a stack para realizar a operação com os dois valores do topo, substitui esses valores do topo pelo resultado da operação e devolve a nova stack
    calculate _ (Stk []) = error "Run-time error"
    calculate _ (Stk [_]) = error "Run-time error"
    calculate "Add" (Stk ((ValorInt x):(ValorInt y):xs)) = Stk ((ValorInt (x+y)):xs)
    calculate "Sub" (Stk ((ValorInt x):(ValorInt y):xs)) = Stk ((ValorInt (x-y)):xs)
    calculate "Mult" (Stk ((ValorInt x):(ValorInt y):xs)) = Stk ((ValorInt (x*y)):xs)
    calculate _ _ = error "Run-time error"

    compare:: String -> Stack -> Stack -- Recebe uma string dizendo o tipo de comparação (booleana) que deseja realizar, recebe a stack para realizar a operação com os dois valores do topo, substitui esses valores do topo pelo resultado da operação e devolve a nova stack
    compare _ (Stk []) = error "Run-time error"
    compare _ (Stk [_]) = error "Run-time error"
    compare "Equ" (Stk ((ValorInt x):(ValorInt y):xs)) = push (ValorBool (x==y)) (Stk xs) 
    compare "Equ" (Stk ((ValorBool x):(ValorBool y):xs)) = push (ValorBool (x==y)) (Stk xs) 
    compare "Le" (Stk ((ValorInt x):(ValorInt y):xs)) = push (ValorBool (x<=y)) (Stk xs)
    compare "And" (Stk ((ValorBool x):(ValorBool y):xs)) = push (ValorBool (x&&y)) (Stk xs)
    compare _ _ = error "Run-time error"

    neg:: Stack -> Stack -- Recebe uma stack, nega o valor booleano no topo da stack, substitui esse valor no topo da stack e devolve a nova stack 
    neg (Stk []) = error "Run-time error"
    neg (Stk ((ValorBool b):xs)) = Stk ((ValorBool (not b)):xs)
    neg _ = error "Run-time error"

    empty :: Stack -- Cria uma stack vazia
    empty = Stk []

    isEmpty :: Stack -> Bool -- Verifica se a stack está vazia e devolve o valor booleano true ou false
    isEmpty (Stk []) = True
    isEmpty (Stk _) = False

    instance Show Stack where -- Representação da stack no formato string
        show (Stk []) = ""
        show (Stk [x]) = show x
        show (Stk (x:xs)) = show x ++ "," ++ show (Stk xs)
