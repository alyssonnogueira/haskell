data AExp = 	Num Int
	       	|Var String
		|Som AExp AExp
	        |Sub AExp AExp
		|Mul AExp AExp
    deriving(Show)

data BExp =	 TRUE
		| FALSE
	        | Not BExp
		| And BExp BExp
	        | Or  BExp BExp
		| Ig  AExp AExp
    deriving(Show)

data CExp =    While BExp CExp
		| If BExp CExp CExp
		| Seq CExp CExp
		| Atrib AExp AExp
	        | Skip
		| DoWhile CExp BExp
		| Repeat CExp BExp
		| Throw
		| TryCatch CExp CExp
	deriving(Show)
                
data Tipo =	INT
		|BOOL
		|ERRO
		|UNIT
	deriving(Show)

aVerificaTipo :: AExp -> Tipo

--Num
aVerificaTipo (Num n) = INT
--Var
aVerificaTipo (Var x) = INT
--Som
aVerificaTipo (Som e1 e2) = case (aVerificaTipo e1 )of
					INT-> case (aVerificaTipo e2 )of
							INT -> INT
							_ -> ERRO
					_ ->ERRO
--Sub
aVerificaTipo (Sub e1 e2) = case (aVerificaTipo e1 )of
					INT-> case (aVerificaTipo e2 )of
							INT -> INT
							_ -> ERRO
					_ ->ERRO
--Mul
aVerificaTipo (Mul e1 e2) = case (aVerificaTipo e1 )of
					INT-> case (aVerificaTipo e2 )of
							INT -> INT
							_ -> ERRO
					_ ->ERRO


bVerificaTipo :: BExp -> Tipo
bVerificaTipo (TRUE) = BOOL
bVerificaTipo (FALSE) = BOOL
--And
bVerificaTipo (And e1 e2) = case (bVerificaTipo e1)of
					BOOL-> case (bVerificaTipo e2)of
							BOOL -> BOOL
							_ -> ERRO
					_ ->ERRO
--Or
bVerificaTipo (Or e1 e2) = case (bVerificaTipo e1)of
					BOOL-> case (bVerificaTipo e2)of
							BOOL -> BOOL
							_ -> ERRO
					_ ->ERRO
--Ig
bVerificaTipo (Ig e1 e2) = case (aVerificaTipo e1)of
					INT-> case (aVerificaTipo e2 )of
							INT -> INT
							_ -> ERRO
					_ ->ERRO
cVerificaTipo :: CExp -> Tipo
cVerificaTipo (Skip) = UNIT
cVerificaTipo (Throw) = UNIT
--If
cVerificaTipo (If TRUE e1 e2) = case (bVerificaTipo TRUE)of
					BOOL-> case (cVerificaTipo e2)of
							UNIT -> UNIT
							_ ->ERRO
cVerificaTipo (If FALSE e1 e2) = case (bVerificaTipo FALSE)of
					BOOL-> case (cVerificaTipo e2)of
							UNIT -> UNIT
							_ ->ERRO
cVerificaTipo (If b e1 e2) = case (bVerificaTipo b)of
					BOOL-> case (cVerificaTipo e1)of
							UNIT -> case (cVerificaTipo e2)of
									UNIT->UNIT
									_ ->ERRO
							_ ->ERRO
					_ ->ERRO
--Seq
cVerificaTipo (Seq Skip e) = case (cVerificaTipo Skip)of
					UNIT-> case (cVerificaTipo e)of
							UNIT->UNIT
							_ ->ERRO
					_ ->ERRO
cVerificaTipo (Seq e1 e2) = case (cVerificaTipo e1)of
					UNIT-> case (cVerificaTipo e2)of
							UNIT->UNIT
							_ ->ERRO
					_ ->ERRO
--While
cVerificaTipo (While b e) = case (bVerificaTipo b)of
					BOOL-> case (cVerificaTipo e)of
							UNIT->UNIT
							_ ->ERRO
					_ ->ERRO
--Atrib
cVerificaTipo (Atrib x y) = case (aVerificaTipo x)of
					INT-> case (aVerificaTipo y)of
							INT->UNIT
							_ ->ERRO
					_ ->ERRO
--DoWhile
cVerificaTipo (DoWhile e b) = case (cVerificaTipo e)of
					UNIT-> case (bVerificaTipo b)of
						BOOL->UNIT
						_ ->ERRO
					_ ->ERRO
--Repeat
cVerificaTipo (Repeat e b) = case (cVerificaTipo e)of
					UNIT-> case (bVerificaTipo b)of
						BOOL->UNIT
						_ ->ERRO
					_ ->ERRO
--TryCatch
cVerificaTipo (TryCatch Throw c) = case (cVerificaTipo Throw)of
					UNIT-> case (cVerificaTipo c)of
						UNIT->UNIT
						_ ->ERRO
					_ ->ERRO
cVerificaTipo (TryCatch Skip c) = case (cVerificaTipo Skip)of
					UNIT-> case (cVerificaTipo c)of
						UNIT->UNIT
						_ ->ERRO
					_ ->ERRO
cVerificaTipo (TryCatch e1 e2) = case (cVerificaTipo e1)of
					UNIT-> case (cVerificaTipo e2)of
						UNIT->UNIT
						_ ->ERRO
					_ ->ERRO

