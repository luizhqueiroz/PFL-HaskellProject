module Value where
    data Value = ValorInt Integer | ValorBool Bool -- Representação de um valor que pode ser inteiro ou booleano

    instance Show Value where -- Representação do Value no formato string
        show (ValorInt x) = show x
        show (ValorBool b) = show b

    instance Eq Value where -- Representa como é feita a igualdade entre Values
        (ValorInt x) == (ValorInt y) = x == y
        (ValorBool x) == (ValorBool y) = x == y
        _ == _ = False
