-----------------------------------------------------------------------
---------------------------- Jonathan Sias ----------------------------
-----------------------------------------------------------------------

-----------------------------------------------------------------------
--------------------------- Semantica Formal --------------------------
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
		| If E E E 
		| While E E 
		| Atrib E E 
		| Seq E E 
		| ReapeatUntil E E 
		| TryCatch E E E 
		| DoWhile E E 
		| Exec E E 
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
													_ -> error(show b2 ++ " deveria ter tipo BOOL e possui tipo: " ++ show tb2)
									_ -> error(show b1 ++ " deveria ter tipo BOOL e possui tipo: " ++ show tb1)
--OR
verificaTipos (OR b1 b2) = let tb1 = verificaTipos b1
								in case tb1 of
									BOOL -> let tb2 = verificaTipos b2
												in case tb2 of
													BOOL -> BOOL
													_ -> error(show b2 ++ " deveria ter tipo BOOL e possui tipo: " ++ show tb2)
									_ -> error(show b1 ++ " deveria ter tipo BOOL e possui tipo: " ++ show tb1)
--IG
verificaTipos (Ig e1 e2) = let te1 = verificaTipos e1
								in case te1 of
									INT -> let te2 = verificaTipos e2
												in case te2 of
													INT -> INT
													_ -> error(show e2 ++ " deveria ter tipo INT e possui tipo: " ++ show te2)
									_ -> error(show e1 ++ " deveria ter tipo INT e possui tipo: " ++ show te1)
--IF
verificaTipos (If b e1 e2) = let tb = verificaTipos b
									in case tb of
										BOOL -> let te1 = verificaTipos e1
													in case te1 of
														UNIT -> let te2 = verificaTipos e2
																	in case te2 of
																		UNIT -> UNIT
																		_ -> error(show e2 ++ " deveria ter tipo UNIT e possui tipo: " ++ show te2)
														_ -> error(show e1 ++ " deveria ter tipo UNIT e possui tipo: " ++ show te1)
										_ -> error(show b ++ " deveria ter tipo BOOL e possui tipo: " ++ show tb)
--WHILE
verificaTipos (While b e) = let tb = verificaTipos b
								in case tb of
									BOOL -> let te = verificaTipos e
												in case te of
													UNIT -> UNIT
													_ -> error(show e ++ " deveria ter tipo UNIT e possui tipo: " ++ show te)
									_ -> error(show b ++ " deveria ter tipo BOOL e possui tipo: " ++ show tb)
--ATRIB
verificaTipos (Atrib e1 e2) = let te1 = verificaTipos e1
								in case te1 of
									INT -> let te2 = verificaTipos e2
												in case te2 of
													INT -> UNIT
													_ -> error(show e2 ++ " deveria ter tipo INT e possui tipo: " ++ show te2)
									_ -> error(show e1 ++ " deveria ter tipo INT e possui tipo: " ++ show te1)
--SEQ
verificaTipos (Seq e1 e2) = let te1 = verificaTipos e1
								in case te1 of
									UNIT -> let te2 = verificaTipos e2
												in case te2 of
													UNIT -> UNIT
													_ -> error(show e2 ++ " deveria ter tipo UNIT e possui tipo: " ++ show te2)
									_ -> error(show e1 ++ " deveria ter tipo UNIT e possui tipo: " ++ show te1)
--REPEAT
verificaTipos (ReapeatUntil e b) = let te = verificaTipos e
										in case te of
											UNIT -> let tb = verificaTipos b
														in case tb of
															BOOL -> UNIT
															_ -> error(show b ++ " deveria ter tipo BOOL e possui tipo: " ++ show tb)
											_ -> error(show b ++ " deveria ter tipo UNIT e possui tipo: " ++ show te)
--TRYCATCH
verificaTipos (TryCatch e1 e2 e3) = let te1 = verificaTipos e1
										in case te1 of
											UNIT -> let te2 = verificaTipos e2
														in case te2 of
															UNIT -> let te3 = verificaTipos e3
																		in case te3 of
																			UNIT -> UNIT
																			_ -> error(show e3 ++ " deveria ter tipo UNIT e possui tipo: " ++ show te3)
															_ -> error(show e2 ++ " deveria ter tipo UNIT e possui tipo: " ++ show te2)
											_ -> error(show e1 ++ " deveria ter tipo UNIT e possui tipo: " ++ show te1)
--DOWHILE
verificaTipos (DoWhile e b) = let te = verificaTipos e
								in case te of
									UNIT -> let tb = verificaTipos b
												in case tb of
													BOOL -> UNIT
													_ -> error(show b ++ " deveria ter tipo BOOL e possui tipo: " ++ show tb)
									_ -> error(show e ++ " deveria ter tipo UNIT e possui tipo: " ++ show te)
--EXEC
verificaTipos (Exec c e) = let tc = verificaTipos c
								in case tc of
									UNIT -> let te = verificaTipos e
												in case te of
													INT -> INT
													_ -> error(show e ++ " deveria ter tipo INT e possui tipo: " ++ show te)
									_ -> error(show c ++ " deveria ter tipo UNIT e possui tipo: " ++ show tc)

------------------------------------------------------------------------
------------------------------- EXEMPLOS -------------------------------
------------------------------------------------------------------------

--num
num::E
num = (Num 10)

--var
var::E
var = (Var "x")

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

--ex9 (correto)
ex9::E
ex9 = Ig (Soma (Num 4) (Num 2)) (Num 6)

--ex10 (vai dar erro)
ex10::E
ex10 = Ig (TRUE) (Num 2)

---------------
--exemplos IF--
---------------
-- CORRETOS:
exif1::E
exif1 = If (TRUE) (Atrib (Num 2) (Num 2)) (Atrib (Num 1) (Num 4))

exif2::E
exif2 = If (FALSE) (exif1) (exif1)

exif3::E
exif3 = If (ex7) (exif1) (exif2)

-- ERROS:
exif4::E
exif4 = If (TRUE) (Soma (Num 8) (Var "x")) (Num 2)

exif5::E
exif5 = If (Num 3) (Soma (Num 4) (Var "x")) (TRUE)

exif6::E
exif6 = If (FALSE) (exif3) (TRUE)

------------------
--exemplos WHILE--
------------------
-- CORRETOS:
exw1::E
exw1 = While (ex5) (If (TRUE) (Atrib (Num 11) (Var "x")) (exif3))

exw2::E
exw2 = While (TRUE) (Atrib (Var "x") (Var "y"))

exw3::E
exw3 = While (FALSE) (exif1)

-- ERROS:
exw4::E
exw4 = While (Num 7) (Atrib (Var "z") (Num 13))

exw5::E
exw5 = While (TRUE) (Mult (Var "y") (Var "z"))

exw6::E
exw6 = While (FALSE) (Sub (Num 10) (Num 10))

------------------
--exemplos ATRIB--
------------------
-- CORRETOS:
exat1::E
exat1 = Atrib (Sub (Num 20) (Var "x")) (Mult (Soma (Var "y") (Var "z")) (Soma (Num 7) (Num 9)))

exat2::E
exat2 = Atrib (Ig (Soma (Num 1) (Num 23)) (Sub (Var "a") (Var "b"))) (Ig (Mult (Num 2) (Num 6)) (Mult (Var "z") (Var "x")))

exat3::E
exat3 = Atrib (Ig (Soma (Ig (Num 21) (Var "y")) (Ig (Var "b") (Var "c"))) (Sub (Num 31) (Num 22))) (Ig (Soma(Var "x") (Var "y")) (Mult (Num 1) (Var "c")))

-- ERROS:
exat4::E
exat4 = Atrib (Soma (Ig (Num 2) (Var "b")) (Ig (Var "a") (Var "c"))) (TRUE)

exat5::E
exat5 = Atrib (OR (FALSE) (TRUE)) (AND (OR (TRUE) (TRUE)) (AND (FALSE) (FALSE)))

exat6::E
exat6 = Atrib (exat3) (TRUE)

-----------------
-- exemplos SEQ--
-----------------
-- CORRETOS:
exseq1::E
exseq1 = Seq (If (AND (TRUE) (FALSE)) (Atrib (Soma (Var "z") (Num 2)) (Mult (Num 4) (Var "y"))) (Atrib (Num 4) (Num 2))) (While (TRUE) (exat3))

exseq2::E
exseq2 = Seq (DoWhile (exat2) (AND (TRUE) (TRUE))) (TryCatch (DoWhile (exseq1) (TRUE)) (If (FALSE) (Atrib (Num 0) (Num 0)) (exseq1)) (While (TRUE) (exif3)))

exseq3::E
exseq3 = Seq (TryCatch (exseq1) (exseq2) (While (TRUE) (exseq2))) (If (TRUE) (DoWhile (exseq2) (FALSE)) (While (FALSE) (exat1)))

-- ERROS:
exseq4::E
exseq4 = Seq (If (TRUE) (exat2) (exat3)) (AND (FALSE) (FALSE))

exseq5::E
exseq5 = Seq (Mult (Var "y") (Var "c")) (TRUE)

exseq6::E
exseq6 = Seq (exif2) (FALSE)

-------------------
--exemplos REPEAT--
-------------------
-- CORRETOS:
exru1::E
exru1 = ReapeatUntil (If (OR (AND (TRUE) (TRUE)) (AND (FALSE) (FALSE))) (exif2) (exseq1)) (AND (OR (TRUE) (FALSE)) (OR (FALSE) (TRUE)))

exru2::E
exru2 = ReapeatUntil (While (AND (OR (TRUE) (TRUE)) (OR (TRUE) (FALSE))) (exseq2)) (AND (OR (FALSE) (FALSE)) (AND (TRUE) (FALSE)))

exru3::E
exru3 = ReapeatUntil (DoWhile (exseq1) (AND (OR (FALSE) (TRUE)) (OR (TRUE) (FALSE)))) (OR (AND (TRUE) (FALSE)) (OR (TRUE) (FALSE)))

-- ERROS:
exru4::E
exru4 = ReapeatUntil (Atrib (Var "a") (Var "c")) (AND (FALSE) (Soma (Num 10) (Num 2)))

exru5::E
exru5 = ReapeatUntil (Soma (Var "x") (Num 4)) (FALSE)

exru6::E
exru6 = ReapeatUntil (Mult (Sub (Num 4) (Num 2)) (Var "y")) (TRUE)

----------------
--exemplo EXEC--
----------------
--CORRETO:
exemploexec1::E
exemploexec1 = Exec (Atrib (Soma (Num 1) (Num 2)) (Mult (Num 2) (Num 2))) (Sub (Num 4) (Num 2))
--ERRADO:
exemploexec2::E
exemploexec2 = Exec (While (AND (TRUE) (FALSE)) (Atrib (Soma (Num 4) (Num 4)) (Sub (Num 2) (Num 1)))) (Atrib (Mult (Num 2) (Num 4)) (Var "y"))