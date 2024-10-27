module State (State, store, fetch, empty, isEmpty, stateSort) where
    import Value

    data State = Stt [(String, Value)] -- Representação do state

    store:: (String, Value) -> State -> State -- Verifica se já existe algum state com a String recebida, se sim, substituiu o Value pelo novo Value recebido, caso contrário, guarda o novo tuple (String, Value) no state e devolve esse novo state
    store (s,v) (Stt xs) = Stt (change (s,v) xs)

    fetch:: String -> State -> Value -- Recebe uma string, recebe um state e procura nesse state pelo Value correspondente ao tuple que possui a String recebida e retorna esse Value
    fetch f (Stt []) = error "Run-time error"
    fetch f (Stt ((s,v):xs))
        | f == s = v
        | otherwise = fetch f (Stt xs)

    empty:: State -- Cria um state vazio
    empty = Stt [] 

    isEmpty :: State -> Bool -- Verifica se o state está vazio e devolve o valor booleano true ou false
    isEmpty (Stt []) = True
    isEmpty (Stt _) = False

    stateSort :: State -> State -- Recebe um state e ordena o state por ordem alfabética das Strings referentes ao tuple (String, Value) e devolve o state ordenado
    stateSort (Stt xs) = Stt (qsort xs)

    change:: (String, Value) -> [(String, Value)] -> [(String, Value)] -- Recebe um tuple (String, Value) e uma lista de tuples (String, Value), verifica se na lista recebida já existe um tuple com a mesma String recebida, se sim, substituiu o Value pelo novo Value recebido, caso contrário, guarda o novo tuple (String, Value) na lista e devolve a nova lista
    change (s,v) [] = [(s,v)]
    change (s,v) ((x,y):xs) 
        | s == x = ((s,v):xs)
        | otherwise = [(x,y)] ++ (change (s,v) xs) 

    qsort :: [(String, Value)] -> [(String, Value)] -- Receb uma lista de tuple (String, Value), ordena por ordem alfabética das Strings essa lista e devolve a nova lista
    qsort [] = []
    qsort ((s,v):xs) = qsort menores ++ [(s,v)] ++ qsort maiores
        where menores = [(y,z) | (y,z)<-xs, y<=s]
              maiores = [(y,z) | (y,z)<-xs, y>s]

    instance Show State where -- Representação do state no formato string
        show (Stt []) = ""
        show (Stt [(s, v)]) = s ++ "=" ++ show v
        show (Stt ((s,v):xs)) = s ++ "=" ++ show v ++ "," ++ show (Stt xs)
