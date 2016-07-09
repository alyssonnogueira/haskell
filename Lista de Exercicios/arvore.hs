data Arvore = Folha Int | No Int Arvore Arvore
	deriving(Eq, Show)

arv1::Arvore
arv1 = No 10 (Folha 2) (No 19 (Folha 17) (Folha 20))
arvoreLista :: Arvore -> [Int]
arvoreLista (Folha n) = [n]
arvoreLista (No n a1 a2) = n: (arvoreLista a1 ++ arvoreLista a2)

contaArvore::Arvore -> Int
contaArvore (Folha n) = 1
contaArvore (No n a1 a2) = 1 + contaArvore (a1) + contaArvore(a2)

reflete::Arvore -> Arvore
reflete (Folha n) = Folha n
reflete (No n a1 a2) = No n (reflete a2) (reflete a1)

data E = Num Int | Soma E E | Mult E E | IF B E E
	deriving (Eq, Show)

prog1 :: E
prog1 = Soma (Mult (Num 3) (Num 4)) (Num 5)

prog2::E
prog2 = Mult (Soma (Num 2) (Num 3)) (Soma (Num 4) (Num 9)) 

bigStepE :: E -> Int
bigStepE (Num n) = n
bigStepE (Soma e1 e2) = (bigStepE e1) + (bigStepE e2)
bigStepE (Mult e1 e2) = (bigStepE e1) * (bigStepE e2)
bigStepE (IF b e1 e2) 
	| bigStepB b = bigStepE e1
	| otherwise = bigStepE e2
data B = TRUE | FALSE | Not B | And B B | Or B B | LEQ E E
	deriving(Eq, Show)

bigStepB :: B -> Bool
bigStepB (TRUE) = True
bigStepB (FALSE) = False
bigStepB (Not b) = not (bigStepB b)
bigStepB (And b1 b2) 
 	| (bigStepB b1) = (bigStepB b2)
	| otherwise = False
bigStepB (Or b1 b2)
	| (bigStepB b1) = True
	| (bigStepB b1) = (bigStepB b2)
bigStepB (LEQ e1 e2) = (bigStepE) e1 <= (bigStepE e2)


prog3::B
prog3 = Or (TRUE) FALSE

progIF:: E 
progIF = Soma (Num 2) 
	(IF (LEQ (Soma (Num 1) (Num 1)) 
		(Soma (Num 4) (Num 4))) 
		(Soma (Num 2) (Num 2)) 
		(Soma (Num 3) (Num 3)))


-- Pra casa definir 
-- E := n 
--	| PRED E 
--	| SUC E | IF B THEN E ELSE E
-- B:= ISZERO E