--Jonathan Sias


import Estado


data AExp = 	Num Int
				|Var String
				|Som AExp AExp
                |Sub AExp AExp
				|Mul AExp AExp
				deriving(Show)

data BExp =	 	TRUE
				| FALSE
                | Not BExp
				| And BExp BExp
                | Or  BExp BExp
				| Ig  AExp AExp
				deriving(Show)

data CExp =    	While BExp CExp
				| If BExp CExp CExp
				| Seq CExp CExp
				| Atrib AExp AExp
                | Skip
				deriving(Show)                



abigStep :: (AExp,Estado) -> (Int,Estado)
--VAR
abigStep (Var x,s) = (procuraVar s x,s)
--NUM
abigStep (Num n,s) = (n,s)
--SOMA
abigStep (Som e1 e2,s)  = let	(n1,s1) = abigStep (e1, s)
				(n2,s2) = abigStep (e2, s)
					in (n1+n2,s)
--SUB
abigStep (Sub e1 e2,s)  = let   (n1,s1) = abigStep (e1, s)
				(n2,s2) = abigStep (e2, s)
					in (n1-n2,s)
--MULT
abigStep (Mul e1 e2,s)  = let   (n1,s1) = abigStep (e1, s)
				(n2,s2) = abigStep (e2, s)
					in (n1*n2,s)


bbigStep :: (BExp,Estado) -> (Bool,Estado)
--TRUE
bbigStep (TRUE,s)  	= (True,s)
--FALSE
bbigStep (FALSE,s) 	= (False,s)
--NOT
bbigStep (Not b,s) 	= let 	(b1,s1) = bbigStep (b,s)
					in (not b1,s1)
--IG
bbigStep (Ig e1 e2,s )  = let 	(n1,s1) = abigStep (e1,s)
			 	(n2,s2) = abigStep (e2,s)
					in (n1 == n2, s)
--AND
--bbigStep (And b1 b2,s )  = let (b1,s) = bbigStep (b1, s)
--				in case b1 of
--					True-> let		(_,s) = bbigStep (
--OR


cbigStep :: (CExp,Estado) -> (CExp,Estado)
cbigStep (Skip,s)      	= (Skip,s)
cbigStep (While b c, s) = let (b1,s1) = bbigStep (b,s)
				in case b1 of
					True -> let 	(_,s2) = cbigStep (c,s)
							(_,s3) = cbigStep (While b c,s2)
							in (Skip,s3)
					False -> (Skip,s)
--cbigStep (If b c1 c2,s) = 
--cbigStep (Seq c1 c2,s)  = 
--cbigStep (Atrib (Var x) e,s) = 


meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]


exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

--TESTES

teste1 :: BExp
teste1 = (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3)))

teste2 :: BExp
teste2 = (Ig (Som (Var "x") (Num 3))  (Mul (Num 2) (Num 3)))


--testec1 :: CExp
--testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
--		(Atrib (Var "y") (Var "z")))

--fatorial :: CExp
--fatorial = (Seq (Atrib (Var "y") (Num 1))
--                (While (Not (Ig (Var "x") (Num 1)))
--                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
--                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
