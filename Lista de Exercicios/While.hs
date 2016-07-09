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
		| Do CExp
		| Repeat CExp
		| Loop AExp CExp
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
cbigStep (Skip,s)      	= (Skip,s)
cbigStep (While b c, s) = let (b1,s1) = bbigStep (b,s)
				in case b1 of
					True -> let (_,s2) = cbigStep (c,s) 
						(_,s3) = cbigStep (While b c,s2) 
							in (Skip,s3)
					False -> (Skip,s)
cbigStep (If b c1 c2,s) = let (b1, s) = bbigStep (b, s) 
                in case b1 of 
					True -> let (calc1, s2) = cbigStep (c2, s)
							in (Skip, s2)
					False -> let (calc2, s2) = cbigStep (c2, s)
							in (Skip, s2)		
cbigStep (Atrib (Var x) e,s) = let (Var x1) = abigStep(x, s)
						(Var e1) = abigStep(e, s)
						in (Skip, s[x = e1])
--cbigStep (Do CExp While BExp) =
--cbigStep (Repeat CExp Until BExp) =
--cbigStep (Loop AExp CExp) =
--X,y := E1, E2

exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

exemplosub :: AExp
exemplosub = Sub (Num 3) (Sub (Var "x") (Var "y"))

exemplomult :: AExp
exemplomult = Mul (Num 3) (Mul (Var "x") (Var "y"))

teste1 :: BExp
teste1 = (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3)))
teste2 :: BExp
teste2 = (And (And (TRUE) (FALSE))  (Or (TRUE) (FALSE)))


testec1 :: CExp
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
		(Atrib (Var "y") (Var "z")))
testec2 :: CExp
testec2 = (If (And (TRUE) (FALSE)) (Som (Num 3) (Num 3)) (Mul (Num 2) (Num 3)))
fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
