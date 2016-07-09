{-------------------------------------------------------------------
TRABALHO 2: Implementar um analisador semântico para a linguagem
            imperativa, ou seja, um verificador de tipos para as
            expressões.
Nome: Alan Romagnoli.
-------------------------------------------------------------------}

-- Tipo Algébrico das Expressões Aritméticas.
data AExp = Num Int | Var String | Som AExp AExp | Sub AExp AExp | Mul AExp AExp
            deriving(Eq, Show)

-- Tipo Algébrico das Expressões Booleanas.
data BExp =	TRUE | FALSE | Not BExp	| And BExp BExp | Or  BExp BExp | Ig  AExp AExp
            deriving(Eq, Show)

-- Tipo Algébrico dos Comandos.
data CExp = While BExp CExp | If BExp CExp CExp | Seq CExp CExp | Atrib AExp AExp | Skip |
            DoWhile BExp CExp | RepeatUntil CExp BExp | Throw | TryCatch CExp CExp CExp
            deriving(Eq, Show)
            
-- Tipos possíveis das expressões.
data Tipo = INT | BOOL | UNIT | ERRO
            deriving (Eq, Show)

{---------------------------------------------------------------------------------------------}
-- Verifica o tipo as expressões aritméticas.
verificaExpA :: AExp -> Tipo
verificaExpA (Num n) = INT
verificaExpA (Var s) = INT
-- Verifica se exp1 é INT para então verificar se exp2 também o é.
verificaExpA (Som exp1 exp2) =
  case (verificaExpA exp1) of
    INT -> case (verificaExpA exp2) of
            INT -> INT
            _ -> ERRO
    _ -> ERRO -- Clausula Default.
verificaExpA (Sub exp1 exp2) =
  case (verificaExpA exp1) of
    INT -> case (verificaExpA exp2) of
            INT -> INT
            _ -> ERRO
    _ -> ERRO
verificaExpA (Mul exp1 exp2) =
  case (verificaExpA exp1) of
    INT -> case (verificaExpA exp2) of
            INT -> INT
            _ -> ERRO
    _ -> ERRO

{---------------------------------------------------------------------------------------------}

-- Verifica o tipo das expressões booleanas.
verificaExpB :: BExp -> Tipo
verificaExpB (TRUE) = BOOL
verificaExpB (FALSE) = BOOL
verificaExpB (Not b) = BOOL
verificaExpB (And b1 b2) =
  case (verificaExpB b1) of
    BOOL -> case (verificaExpB b2) of
              BOOL -> BOOL
              _ -> ERRO
    _ -> ERRO
verificaExpB (Or b1 b2) =
  case (verificaExpB b1) of
    BOOL -> case (verificaExpB b2) of
              BOOL -> BOOL
              _ -> ERRO
    _ -> ERRO
verificaExpB (Ig exp1 exp2) =
  case (verificaExpA exp1) of
    INT -> case (verificaExpA exp2) of
            INT -> BOOL
            _ -> ERRO
    _ -> ERRO

{---------------------------------------------------------------------------------------------}

-- Verifica o tipo das expressões de comandos.
verificaExpC :: CExp -> Tipo
verificaExpC (Skip) = UNIT
verificaExpC (Throw) = UNIT
verificaExpC (While b c) =
  case (verificaExpB b) of
    BOOL -> case (verificaExpC c) of
              UNIT -> UNIT
              _ -> ERRO
    _ -> ERRO
verificaExpC (If b c1 c2) =
  case (verificaExpB b) of
    BOOL -> case (verificaExpC c1) of
              UNIT -> case (verificaExpC c2) of
                        UNIT -> UNIT
                        _ -> ERRO
              _ -> ERRO
    _ -> ERRO
verificaExpC (Seq c1 c2) =
  case (verificaExpC c1) of
    UNIT -> case (verificaExpC c2) of
              UNIT -> UNIT
              _ -> ERRO
    _ -> ERRO
verificaExpC (Atrib exp1 exp2) =
  case (verificaExpA exp1) of
    INT -> case (verificaExpA exp2) of
            INT -> UNIT
            _ -> ERRO
    _ -> ERRO
verificaExpC (DoWhile b c) =
  case (verificaExpB b) of
    BOOL -> case (verificaExpC c) of
              UNIT -> UNIT
              _ -> ERRO
    _ -> ERRO
verificaExpC (RepeatUntil c b) =
  case (verificaExpC c) of
    UNIT -> case (verificaExpB b) of
              BOOL -> UNIT
              _ -> ERRO
    _ -> ERRO
verificaExpC (TryCatch c1 c2 c3) =
  case (verificaExpC c1) of
    UNIT -> case (verificaExpC c2) of
              UNIT -> case (verificaExpC c3) of
                        UNIT -> UNIT
                        _ -> ERRO
              _ -> ERRO
    _ -> ERRO

{---------------------------------------------------------------------------------------------}

-- Dados de entrada para testes.
testeExpA1 :: AExp
testeExpA2 :: AExp
testeExpA3 :: AExp
testeExpA4 :: AExp
testeExpA5 :: AExp

testeExpA1 = (Num 5)
testeExpA2 = (Var "x")
testeExpA3 = (Som (Num 4) (Var "y"))
testeExpA4 = (Sub (Var "x") (Num 3))
testeExpA5 = (Mul testeExpA3 testeExpA4)

testeExpB1 :: BExp
testeExpB2 :: BExp
testeExpB3 :: BExp
testeExpB4 :: BExp
testeExpB5 :: BExp

testeExpB1 = TRUE
testeExpB2 = (Not testeExpB1)
testeExpB3 = (And testeExpB1 testeExpB2)
testeExpB4 = (Or testeExpB3 testeExpB1)
testeExpB5 = (Ig testeExpA4 testeExpA2)

testeExpC1 :: CExp
testeExpC2 :: CExp
testeExpC3 :: CExp
testeExpC4 :: CExp
testeExpC5 :: CExp
testeExpC6 :: CExp
testeExpC7 :: CExp
testeExpC8 :: CExp
testeExpC9 :: CExp

testeExpC1 = (While testeExpB2 Skip)
testeExpC2 = (If testeExpB2 testeExpC1 testeExpC1)
testeExpC3 = (Seq testeExpC2 testeExpC1)
testeExpC4 = (Atrib testeExpA2 testeExpA1)
testeExpC5 = Skip
testeExpC6 = (DoWhile testeExpB3 testeExpC1)
testeExpC7 = (RepeatUntil testeExpC4 testeExpB5)
testeExpC8 = Throw
testeExpC9 = (TryCatch testeExpC2 testeExpC8 testeExpC1)
