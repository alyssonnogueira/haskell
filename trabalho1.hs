-------------------------TRABALHO 1 DONE!  ALYSSON NOGUEIRA SEMANTICA FORMAL --------------------------------------
import Estado


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
		| DoWhile BExp CExp
		| RepeatUntil CExp BExp
		| Loop AExp CExp
		| DuplaAtrib AExp AExp AExp
                | Skip
	deriving(Show)                

meuEstado :: Estado
meuEstado = [("x",3), ("y",5), ("z",0)]

abigStep :: (AExp,Estado) -> (Int,Estado)
abigStep (Var x,s) = (procuraVar s x,s)
abigStep (Num n,s) = (n,s)
abigStep (Som e1 e2,s)  = let	(n1,s1) = abigStep (e1, s)
				(n2,s2) = abigStep (e2, s)
					in (n1+n2,s)
abigStep (Sub e1 e2,s)  = let	(n1,s1) = abigStep (e1, s)
				(n2,s2) = abigStep (e2, s)
					in (n1-n2,s)
abigStep (Mul e1 e2,s)  = let	(n1, s1) = abigStep (e1, s)
				(n2, s2) = abigStep(e2, s)
					in (n1*n2, s)

bbigStep :: (BExp,Estado) -> (Bool,Estado)
bbigStep (TRUE,s)  	= (True,s)
bbigStep (FALSE,s) 	= (False,s)
bbigStep (Not b,s) 	= let 	(b1,s1) = bbigStep (b,s)
					in (not b1,s1) 
bbigStep (Ig e1 e2,s )  = let 	(n1,s1) = abigStep (e1,s)
			 	(n2,s2) = abigStep (e2,s)
					in (n1 == n2, s)
bbigStep (And b1 b2,s )  = let 	(bool1,s1) = bbigStep (b1,s)
			 	in case bool1 of
			 		True -> let (bool2,s2) = bbigStep (b2,s) 
							in (bool2, s)
					False -> (False, s)
bbigStep (Or b1 b2,s )  = let 	(bool1,s1) = bbigStep (b1,s)
			 	in case bool1 of
			 		True -> (True, s)
			 		False -> let (bool2,s2) = bbigStep (b2,s) 
							in (bool2, s)

cbigStep :: (CExp,Estado) -> (CExp,Estado)
cbigStep (Skip,s) = (Skip,s)
cbigStep (If b c1 c2,s) = let (b1, s1) = bbigStep (b, s) 
                in case b1 of 
					True -> let (calc1, s2) = cbigStep (c2, s1)
							in (Skip, s2)
					False -> let (calc2, s2) = cbigStep (c2, s1)
							in (Skip, s2)
cbigStep (While b c, s) = let (b1,s1) = bbigStep (b,s)
				in case b1 of
					True -> let 	(_,s2) = cbigStep (c,s1)
							(_,s3) = cbigStep (While b c,s2)
							in (Skip,s3)
					False -> (Skip,s)
cbigStep (Atrib (Var var) e,s) = let	(n,s1) = abigStep (e,s);
					s2 = (mudaVar s1 var n)
					in (Skip,s2)
cbigStep (Seq cmd1 cmd2,s)  = let (c1, s1) = cbigStep(cmd1, s); 
					(c2, s2) = cbigStep(cmd2, s1)
					in (Skip, s2)
cbigStep (DoWhile b c, s) = let (c1, s1) = cbigStep(c, s);
					(b2, s2) = bbigStep(b, s1)
				in case b2 of
					True -> let (_, s2) = cbigStep (c, s1);
							(_, s3) = cbigStep (While b c, s2)
							in (Skip, s3)
					False -> (Skip, s1) 
cbigStep (RepeatUntil c b, s) = let (c1, s1) = cbigStep(c, s);
						(b1, s2) = bbigStep(b, s1)
				in case b1 of 
					True -> let (_, s2) = cbigStep(c, s1);
							(_, s3) = cbigStep(RepeatUntil c b, s2)
						    in (Skip, s3)
					False -> (Skip, s2)
cbigStep (Loop a c, s) = let (a1, s1) = abigStep(a, s);
					(c1, s2) = cbigStep(c, s1)
					in (Skip, s2)

cbigStep (DuplaAtrib (Var x) (Var y) e,s) = let	(n, s1) = abigStep (e,s);
									s2 = (mudaVar s1 x n);
									s3 = (mudaVar s2 y n)
									in (Skip,s3)
--DoWhile BExp xp
--RepeatUntil CExp BExp
--Loop AExp CExp
--DuplaAtrib AExp AExp AExp

exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

exemplosub :: AExp
exemplosub = Sub (Num 3) (Sub (Var "x") (Var "y"))

exemplomult :: AExp
exemplomult = Mul (Num 3) (Mul (Var "x") (Var "y"))

--Testa Igual
teste1 :: BExp
teste1 = (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3)))
--Testa And e Or
teste2 :: BExp
teste2 = (And (And (TRUE) (FALSE))  (Or (TRUE) (FALSE)))

--Testa Seq e Atrib
testec1 :: CExp
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
		(Atrib (Var "y") (Var "z")))

-- Testa If e Atrib
testec2 :: CExp
testec2 = (If (And (TRUE) (FALSE)) (Atrib (Var "x") (Som (Num 3) (Num 3))) (Atrib (Var "x") (Mul (Num 2) (Num 3))))

--Testa Varios
fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
--Testa While
testec3 :: CExp
testec3 = (While (Not (Ig (Var "x") (Num 10)))
				(Atrib (Var "x") (Som (Var "x") (Num 1))))
--DO WHILE
testec4 :: CExp
testec4 = (DoWhile (Not (Ig (Var "x") (Num 10)))
				(Atrib (Var "x") (Som (Var "x") (Num 1))))
--REPEAT UNTIL
testec5 :: CExp
testec5 = (DoWhile (Not (Ig (Var "x") (Num 10)))
				(Atrib (Var "x") (Som (Var "x") (Num 1))))
--Testa Dupla Atrib
testec6 :: CExp
testec6 = (DuplaAtrib (Var "y") (Var "z") (Som (Num 3) (Num 3)))

