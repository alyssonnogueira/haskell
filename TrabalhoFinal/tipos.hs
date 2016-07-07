-----------------------------------------------------------------------
---------------------------- Jonathan Sias ----------------------------
-----------------------------------------------------------------------

-----------------------------------------------------------------------
------------------------- Verificação de Tipos ------------------------
-----------------------------------------------------------------------

data E = Num Int
		| Var String 
		| Soma E E 
		| Sub E E 
		| Mult E E 
		| TRUE 
		| FALSE 
		| NOT E 
		| AND E E 
		| OR E E 
		| Ig E E 
		| THROW 
		| SKIP 
		| If E E E 
		| While E E 
		| Atrib E E 
		| Seq E E 
	deriving(Eq, Show)
	
data Tipo = BOOL 
		| INT 
		| UNIT 
		| VOID 
	deriving(Eq, Show)
	
verificaTipos:: E -> Tipo
verificaTipos (Num n) = INT
verificaTipos (Var x) = INT
verificaTipos FALSE = BOOL
verificaTipos TRUE = BOOL
verificaTipos (SKIP) = UNIT
verificaTipos (THROW) = UNIT

--SOMA
verificaTipos (Soma e1 e2) = let te1 = verificaTipos e1 
								in case te1 of
									INT -> let te2 = verificaTipos e2
												in case te2 of
													INT -> INT
													_ -> error(show e2 ++ " deveria ter tipo INT e possui tipo: " ++ show te2)
									_ -> error(show e1 ++ " deveria ter tipo INT e possui tipo: " ++ show te1)
--SUB
verificaTipos (Sub e1 e2) = let te1 = verificaTipos e1 
								in case te1 of
									INT -> let te2 = verificaTipos e2
												in case te2 of
													INT -> INT
													_ -> error(show e2 ++ " deveria ter tipo INT e possui tipo: " ++ show te2)
									_ -> error(show e1 ++ " deveria ter tipo INT e possui tipo: " ++ show te1)
--MULT
verificaTipos (Mult e1 e2) = let te1 = verificaTipos e1 
								in case te1 of
									INT -> let te2 = verificaTipos e2
												in case te2 of
													INT -> INT
													_ -> error(show e2 ++ " deveria ter tipo INT e possui tipo: " ++ show te2)
									_ -> error(show e1 ++ " deveria ter tipo INT e possui tipo: " ++ show te1)
--AND
verificaTipos (AND b1 b2) = let tb1 = verificaTipos b1
								in case tb1 of
									BOOL -> let tb2 = verificaTipos b2
												in case tb2 of
													BOOL -> BOOL
													_ -> error(show b2 ++ " deveria ter tipo BOOL e possui tipo: " ++ show b2)
									_ -> error(show b1 ++ " deveria ter tipo BOOL e possui tipo: " ++ show b1)
--OR
verificaTipos (OR b1 b2) = let tb1 = verificaTipos b1
								in case tb1 of
									BOOL -> let tb2 = verificaTipos b2
												in case tb2 of
													BOOL -> BOOL
													_ -> error(show b2 ++ " deveria ter tipo BOOL e possui tipo: " ++ show b2)
									_ -> error(show b1 ++ " deveria ter tipo BOOL e possui tipo: " ++ show b1)
--IG
verificaTipos (Ig e1 e2) = let te1 = verificaTipos e1
								in case te1 of
									INT -> let te2 = verificaTipos e2
												in case te2 of
													INT -> INT
													_ -> error(show e2 ++ " deveria ter tipo INT e possui tipo: " ++ show e2)
									_ -> error(show e1 ++ " deveria ter tipo INT e possui tipo: " ++ show e1)
--IF
--verificaTipos (If TRUE e1 e2) = let TRUE = verificaTipos TRUE
--									in case TRUE of
--										BOOL -> let te2 = verificaTipos e2
--													in case te2 of
--														UNIT -> UNIT
--														_ -> error(show e2 ++ " deveria ter tipo UNIT e possui tipo: " ++ show e2)
--verificaTipos (If FALSE e1 e2) = let FALSE = verificaTipos FALSE
--									in case FALSE of
--										BOOL -> let te2 = verificaTipos e2
--													in case te2 of
--														UNIT -> UNIT
--														_ ->error(show e2 ++ " deveria ter tipo UNIT e possui tipo: " ++ show e2)
--verificaTipos (If b e1 e2) = let b = verificaTipos b
--								in case b of
--									BOOL -> let te1 = verificaTipos e1
--												in case e1 of
--													UNIT -> let te2 = verificaTipos e2
--																in case e2 of
--																	UNIT -> UNIT
--																	_ -> error(show e2 ++ " deveria ter tipo UNIT e possui tipo: " ++ show e2)
--													_ -> error(show e1 ++ " deveria ter tipo UNIT e possui tipo: " ++ show e1)
--									_ -> error(show b ++ " deveria ter tipo BOOL e possui tipo: " ++ show b)
--WHILE


------------------------------------------------------------------------
------------------------------- EXEMPLOS -------------------------------
------------------------------------------------------------------------

--ex1 (correto)
ex1::E
ex1 = Soma (Soma (Num 1) (Num 2)) (Num 1)
--ex2 (vai dar erro)
ex2::E
ex2 = Soma (Num 2) (Soma TRUE (Num 3))
--ex3 (correto)
ex3::E
ex3 = Sub (Sub (Num 1) (Num 2)) (Num 1)
--ex4 (vai dar erro)
ex4::E
ex4 = Sub (FALSE) (Sub (Num 5) (Num 3))
--ex5 (correto) 
ex5::E
ex5 = AND (FALSE) (TRUE)
--ex6 (vai dar erro)
ex6::E
ex6 = AND (TRUE) (Num 3)
--ex7 (correto)
ex7::E
ex7 = OR (FALSE) (AND (FALSE) (TRUE))
--ex8 (vai dar erro)
ex8::E
ex8 = OR (Sub (Num 2) (Num 4)) (TRUE)
