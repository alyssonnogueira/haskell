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
                | Skip
	deriving(Show)                


meuEstado :: Estado
meuEstado = [("x",6), ("y",0), ("z",0)]




bden :: BExp -> Estado -> Bool
bden TRUE s   		= True 
bden FALSE s   		= False
bden (Not b) s  	= not (bden b s) 
bden (Ig e1 e2) s 	= (aden e1 s) == (aden e2 s)

aden :: AExp -> Estado -> Int
aden (Num n) s	 	= n
aden (Var x) s  	= procuraVar s x
aden (Sub e1 e2) s  	= (aden e1 s) - (aden e2 s)
aden (Som e1 e2) s  	= (aden e1 s) + (aden e2 s)
aden (Mul e1 e2) s  	= (aden e1 s) * (aden e2 s)
 

teste1 :: BExp
teste1 = (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3)))
teste2 :: BExp
teste2 = (Ig (Som (Var "x") (Num 3))  (Mul (Num 2) (Num 3)))

cden :: CExp -> Estado -> Estado
--cden Skip s       		= 
cden (If b c1 c2) s 		= if bden b s then cden c1 s else cden c2 s
--cden (Seq c1 c2) s  		=    
cden (Atrib (Var x) e) s 	= mudaVar s x (aden e s)
cden (Seq c1 c2) s = cden c2 (cden c1 s)
cden (While b c)  s		= (fix w) s
							where
							w g s = if bden b s
										then g (cden c s)
										else s
fix f = f(fix f)


testec1 :: CExp
testec1 = (Atrib (Var "z") (Var "x"))
		

fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
