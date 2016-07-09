-------------------------TRABALHO 2 DONE!  ALYSSON NOGUEIRA SEMANTICA FORMAL --------------------------------------

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
		| TryCatch CExp CExp
                | Skip
                | Throw
	deriving(Show)                



interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False

aSmallStep :: (AExp,Estado) -> (AExp,Estado)
aSmallStep (Var x,s) = (Num (procuraVar s x),s)
--SOMA
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y),s)
aSmallStep (Som (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
					in (Som (Num x) ef,s)
aSmallStep (Som e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
					in (Som ef e2,s)
--SUBTRACAO
aSmallStep (Sub (Num x) (Num y), s) = (Num (x-y),s)
aSmallStep (Sub (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
					in (Sub (Num x) ef,s)
aSmallStep (Sub e1 e2,s)  = let (ef, _) = aSmallStep (e1, s)
					in (Sub ef e2, s)
---MULTIPLICACAO
aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y),s)
aSmallStep (Mul (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
					in (Mul (Num x) ef,s)
aSmallStep (Mul e1 e2,s)  = let (ef, _) = aSmallStep (e1, s)
					in (Mul ef e2, s)

interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False


bSmallStep :: (BExp,Estado) -> (BExp,Estado)
--NOT
bSmallStep (Not FALSE,s) 	= (TRUE,s)
bSmallStep (Not TRUE,s) 	= (FALSE, s)
bSmallStep (Not b, s) = let (bn,sn) = bSmallStep (b,s)
					in (Not bn ,sn)
--AND
bSmallStep (And TRUE b2,s)	= (b2,s)
bSmallStep (And FALSE b2,s)	= (FALSE,s)
bSmallStep (And b1 b2,s)	= let (bn,sn) = bSmallStep (b1,s)
					in (And bn b2,sn)
--OR
bSmallStep (Or TRUE b2,s)	= (TRUE,s)
bSmallStep (Or FALSE b2,s)	= (b2,s)
bSmallStep (Or b1 b2,s)	= let (bn,sn) = bSmallStep (b1,s)
					in (Or bn b2,sn)
-- IGUAL
bSmallStep (Ig (Num n1) (Num n2),s)	= let (b, s1) = (n1 == n2,s);
								in case b of	
									True -> (TRUE, s)
									False -> (FALSE, s)
bSmallStep (Ig (Num n1) a2,s) = let (n2, s1) = aSmallStep (a2,s);
							in (Ig (Num n1) n2, s1)
bSmallStep (Ig a1 a2,s)	= let (n1,s1) = aSmallStep (a1,s);
							in (Ig n1 a2, s1)

interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC (c,s) = if isFinalC c then (c,s) else interpretC (cSmallStep (c,s))

isFinalC :: CExp -> Bool
isFinalC Skip = True 
isFinalC x = False

cSmallStep :: (CExp,Estado) -> (CExp,Estado)
cSmallStep (Skip, s) = (Skip, s) 
cSmallStep (Throw, s) = error ("Ocorreu uma excecao no Codigo")
cSmallStep (Atrib (Var x) (Num n),s) = (Skip, mudaVar s x n)
cSmallStep (Atrib (Var x) e,s) = let (n, s1) = aSmallStep(e, s)
						in (Atrib (Var x) n, s1) 
--Seq
cSmallStep (Seq Throw c2,s)  = (Throw, s)
cSmallStep (Seq Skip c2,s)  = let (cmd2, s1) = cSmallStep(c2, s)
						in (cmd2, s1)
cSmallStep (Seq c1 c2,s)  = let (cmd1, s1) = cSmallStep(c1, s)
						in (Seq cmd1 c2, s1) 
--If
cSmallStep (If FALSE c1 c2,s) = let (cmd2, s1) = cSmallStep(c2, s) 
						in (cmd2, s1)
cSmallStep (If TRUE c1 c2,s) = let (cmd1, s1) = cSmallStep(c1, s)
						in (cmd1, s1)
cSmallStep (If b c1 c2,s) = let (b1, s1) = bSmallStep(b, s)
						in (If b1 c1 c2, s1)
--While
cSmallStep (While b c, s) = (If b (Seq c (While b c)) (Skip), s) 

--TryCatch
cSmallStep(TryCatch Throw c2, s) = let (cmd2, s1) = cSmallStep(c2, s)
						in (cmd2, s)
cSmallStep(TryCatch Skip c2, s) = (Skip, s)
cSmallStep(TryCatch c1 c2, s) = let (cmd1, s1) = cSmallStep(c1, s)
						in (TryCatch cmd1 c2, s)

meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)
testea1 :: AExp
testea1 = Som (Num 3) (Som (Var "x") (Var "y"))
--6
testea2 :: AExp
testea2 = Som (Num 3) (Mul (Var "x") (Var "y"))
--3

-- *Main> interpretB (exemplo2,meuEstado)
testeb1 :: BExp
testeb1 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)
-- (TRUE,[("x",3),("y",0),("z",0)])

--Testa Ig 
testeb2 :: BExp
testeb2 = (Not (Ig (Var "x") (Num 5)))
-- (TRUE,[("x",3),("y",0),("z",0)])

--Testa And e Or
testeb3 :: BExp
testeb3 = (And (And (TRUE) (FALSE))  (Or (TRUE) (FALSE)))
-- (FALSE,[("x",3),("y",0),("z",0)])

-- *Main> interpretC (exemplo3,meuEstado)
--Testa Atrib
testec1 :: CExp
testec1 = (Atrib (Var "y") (Var "x"))
-- (Skip,[("x",3),("y",3),("z",0)])

--Testa Seq e Atrib
testec2 :: CExp
testec2 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
		(Atrib (Var "y") (Var "z")))
-- (Skip,[("x",0),("y",3),("z",3)])

-- Testa If e Atrib
testec3 :: CExp
testec3 = (If (Or (TRUE) (FALSE)) (Atrib (Var "x") (Som (Num 3) (Num 3))) (Atrib (Var "x") (Mul (Num 2) (Num 3))))
-- (Skip,[("x",6),("y",0),("z",0)])

--Testa While
testec4 :: CExp
testec4 = (While (Not (Ig (Var "x") (Num 10)))
				(Atrib (Var "x") (Som (Var "x") (Num 1))))
--Testa TryCatch
testec5 :: CExp
testec5 = (TryCatch testec4 testec3)
testec6 :: CExp
testec6 = (TryCatch Throw testec3)
testec7 :: CExp
testec7 = (Seq Throw testec3)

--Testa Varios
fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
