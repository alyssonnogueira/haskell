{-------------------------------------------------------------------
TRABALHO 1: Implementar as regras smallstep da linguagem imperativa.
Nome: Alan Romagnoli.
-------------------------------------------------------------------}
import Estado

-- Tipo Algébrico das Expressões Aritméticas.
data AExp = Num Int | Var String | Som AExp AExp | Sub AExp AExp | Mul AExp AExp
            deriving(Eq, Show)

-- Tipo Algébrico das Expressões Booleanas.
data BExp =	TRUE | FALSE | Not BExp	| And BExp BExp | Or  BExp BExp | Ig  AExp AExp
            deriving(Eq, Show)

-- Tipo Algébrico dos Comandos.
data CExp = While BExp CExp | If BExp CExp CExp | Seq CExp CExp | Atrib AExp AExp | Skip | DoWhile BExp CExp |
			RepeatUntil CExp BExp | Throw | Try CExp CExp CExp | Catch
            deriving(Eq, Show)

{---------------------------------------------------------------------------------------------}

-- Verifica se continua ou não interpretando a expressão aritmética.
interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a, s) = if isFinalA a then (a, s) else interpretA (aSmallStep (a, s))

-- Verifica se chegou no fim da interpretação aritmética.
isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False

-- Interpreta a expressão aritmética.
aSmallStep :: (AExp, Estado) -> (AExp, Estado)
aSmallStep (Var x, s) = (Num (procuraVar s x), s)
-- Soma.
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y), s)
-- (ef, _) armazena a interpretação de sSmallStep (exp2, s) onde exp2 é (Som (Num x) ef, s)
aSmallStep (Som (Num x) exp2, s) = let (ef, _) = aSmallStep (exp2, s) in (Som (Num x) ef, s)
aSmallStep (Som exp1 exp2, s) = let (ef, _) = aSmallStep (exp1, s) in (Som ef exp2, s)
-- Substração.
aSmallStep (Sub (Num x) (Num y), s) = (Num (x-y), s)
aSmallStep (Sub (Num x) exp2, s) = let (ef, _) = aSmallStep (exp2, s) in (Sub (Num x) ef, s)
aSmallStep (Sub exp1 exp2, s) = let (ef, _) = aSmallStep (exp1, s) in (Sub ef exp2, s)
-- Multiplicação.
aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y), s)
aSmallStep (Mul (Num x) exp2, s) = let (ef, _) = aSmallStep (exp2, s) in (Mul (Num x) ef, s)
aSmallStep (Mul exp1 exp2, s) = let (ef, _) = aSmallStep (exp1, s) in (Mul ef exp2, s)

-- Verifica se continua ou não interpretando a expressão booleana.
interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b, s) = if isFinalB b then (b, s) else interpretB (bSmallStep (b, s))

{---------------------------------------------------------------------------------------------}

-- Verifica se chegou no fim da interpretação booleana.
isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False

-- Interpreta a expressão booleana.
bSmallStep :: (BExp, Estado) -> (BExp, Estado)
-- Not.
bSmallStep (Not FALSE, s) = (TRUE, s)
bSmallStep (Not TRUE, s) = (FALSE, s)
bSmallStep (Not b, s) = let (bn, sn) = bSmallStep (b, s) in (Not bn, sn)
-- And.
bSmallStep (And TRUE b2, s)	= (b2, s)
bSmallStep (And FALSE b2, s) = (FALSE,s)
bSmallStep (And b1 b2, s) = let (bn, sn) = bSmallStep (b1, s) in (And bn b2, sn)
-- Or.
bSmallStep (Or FALSE b2, s) = (b2, s)
bSmallStep (Or TRUE b2, s) = (TRUE, s)
bSmallStep (Or b1 b2, s) = let (bn, sn) = bSmallStep (b1, s) in (Or bn b2, sn)
-- Equal (Igual).
bSmallStep (Ig (Num x) (Num y), s) = if x==y then (TRUE, s) else (FALSE, s)
bSmallStep (Ig (Num x) e2, s) = let (ef, _) = aSmallStep (e2, s) in (Ig (Num x) ef, s)
bSmallStep (Ig e1 e2, s) = let (ef, _) = aSmallStep (e1 ,s) in (Ig ef e2, s)

{---------------------------------------------------------------------------------------------}

-- Verifica se continua ou não interpretando a expressão de comandos.
interpretC :: (CExp, Estado) -> (CExp, Estado)
interpretC (c, s) = if isFinalC c then (c, s) else interpretC (cSmallStep (c, s))

-- Verifica se chegou no fim da interpretação do comando.
isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC x = False

-- Interpreta a expressão de comandos.
cSmallStep :: (CExp,Estado) -> (CExp,Estado)
-- If.
cSmallStep (If TRUE c1 c2, s) = (c1, s)
cSmallStep (If FALSE c1 c2, s) = (c2, s)
cSmallStep (If b c1 c2, s) = let (bn, sn) = bSmallStep (b, s) in (If bn c1 c2, sn)
-- Sequência.
cSmallStep (Seq Skip c2,s) = (c2, s)
cSmallStep (Seq Throw c2, s) = (Throw, s)
cSmallStep (Seq c1 c2, s) = let (cn, sn) = cSmallStep (c1, s) in (Seq cn c2, sn)
-- Atribuição.
cSmallStep (Atrib (Var x) (Num y), s) = (Skip, mudaVar (s) (x) (y))
cSmallStep (Atrib (Var x) e,s) = let (en,sn) = aSmallStep (e,s) in (Atrib (Var x) en,sn)
-- Repeat Until.
cSmallStep (RepeatUntil Skip b, s) = (Skip, s) 
cSmallStep (RepeatUntil c b, s) = let (cn, sn) = cSmallStep(c, s) in (Seq c (If b Skip (RepeatUntil cn b)), s)
-- While
cSmallStep (While b c, s) = (If b (Seq c (While b c)) Skip, s)
-- Do While.
cSmallStep (DoWhile b c, s) = (Seq c (If b Skip (DoWhile b c)), s)
-- Try Skip Catch.
cSmallStep (Try Skip Catch c, s) = (Skip, s) -- Tentar skip significa executar nada.
-- Try Throw Catch.
cSmallStep (Try Throw Catch c, s) = (c, s) -- Lançar uma exceção significa executar o catch!
-- Try com comandos.
cSmallStep (Try c1 Catch c2, s) = let (cn, sn) = cSmallStep(c1, s) in (Try cn Catch c2, sn)

{---------------------------------------------------------------------------------------------}

-- Dados de entrada para testes.
meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]

exemploSoma1 :: AExp
exemploSoma1 = Som (Num 3) (Num 4) -- Resposta 7
exemploSoma2 :: AExp
exemploSoma2 = Som (Num 3) (Som (Var "x") (Var "y")) -- Resposta 6
exemploSoma3 :: AExp
exemploSoma3 = Som (Som (Num 5) (Var "y")) (Som (Var "x") (Num 10)) -- Resposta 18
exemploSub1 :: AExp
exemploSub1 = Sub (Num 3) exemploSoma1 -- Resposta -4
exemploSub2 :: AExp
exemploSub2 = Sub (Num 10) (Som (Var "x") (Num 5)) -- Resposta 2
exemploSub3 :: AExp
exemploSub3 = Sub (Sub (Num 5) (Var "y")) (Sub (Var "x") (Num 10)) -- Resposta 12
exemploMul1 :: AExp
exemploMul1 = Mul (Num 4) (Num 5) -- Resposta 20
exemploMul2 :: AExp
exemploMul2 = Mul (Num 4) (Som (Var "x") (Num 5)) -- Resposta 32
exemploMul3 :: AExp
exemploMul3 = Mul exemploSoma1 exemploSub1 -- Resposta -7
-- Para executar qualquer exemplo acima, executar:
-- ghci> interpretA (exemploSoma1, meuEstado)
-- ghci> interpretA (exemploSub1, meuEstado)
-- ghci> interpretA (exemploMul1, meuEstado)

exemploBool1 :: BExp
exemploBool1 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE) -- Resposta True.
exemploBool2 :: BExp
exemploBool2 = Or (Not exemploBool1) (Or (Not TRUE) FALSE) -- Resposta False.
-- Para executar qualquer exemplo acima, executar:
-- ghci> interpretB (exemploBool1, meuEstado)
-- ghci> interpretB (exemploBool2, meuEstado)

exemploC1 :: CExp
exemploC1 = Atrib (Var "x") (Num 10)
exemploC2 :: CExp
exemploC2 = If FALSE exemploC1 (Atrib (Var "y") (Num 5))
exemploC3 :: CExp
exemploC3 = Try (Seq Throw exemploC1) Catch exemploC2
--cSmallStep (RepeatUntil Skip b, s) = (Skip, s) 
--cSmallStep (RepeatUntil c b, s) = let (cn, sn) = cSmallStep(c, s) in (Seq c (If b Skip (RepeatUntil cn b)), s)
exemploC4 :: CExp
exemploC4 = RepeatUntil (error ("Teste\n")) (If (Ig (Num 3) (Var "x")) Skip (Atrib (Var "x") (Num 3)))
-- Para executar qualquer exemplo acima, executar:
-- ghci> interpretC (exemploC1, meuEstado)
-- ghci> interpretC (exemploC2, meuEstado)
