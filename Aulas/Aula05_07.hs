--Aula dia 5/07
-- Semantica Denotacional  
-- Criada em Oxford nos anos 60
-- A Iéia é mapear construções da linguagem de programação para objetos matemáticos (funções, 
	composição de funções, etc...)
-- Principal Caracteristica: a semantica deve ser composicional.

-- Composicionar: o significado de uma expressão é dado em termos do significado de suas subexpressões.
-- Na prática as definições são parecidas com o que vimos em semântica operacional, a diferença é o significado dos loops.
-- Na semântica operacional, o significado de um loop é dado em termos dele mesmo (recursão) e dessa forma não é composicional.

-- Na semântica denotacional, o significado de um loop é dado por outra função que captura a repetição (ponto fixo)

Semantica Denotacional da Linguagem Imperativa

-- A semantica denotacional da linguagem Imperativa é dada por 3 funções semanticas
				A[| . |] :: ExpArit -> Memória -> Z/
				B[| . |] :: ExpBool -> Memória -> Bool
				C[| . |] :: Comando -> Memória -> Memória

Onde Memória é o 'sigma' usado na semântica operacional:
			sigma = {<x,2>, <y,3>}
Expressões Aritméticas
		E := n | x | E -| E

		A [| n |] sigma = n  (Dnum)
		A [| x |] sigma = sigma(x)  (Dvar)
		A [| E1 + E2 |] sigma = A [| E1 |]sigma + A [| E2 |]sigma (Dsoma)

		A [| (x + 2) + x |] sigma = n  (Dsoma)
		A [| x + 2 |]sigma + A [| x |]sigma = (Dsoma)
	 	A [| x |]sigma + A [| 2 |]sigma + A [| x |]sigma = (Dvar)
	 	2 + A [| 2 |]sigma + 2 = (Dnum)
	 	2 + 2 + 2 = (DSoma)
	 		6

	Booleanos
	 B := True | False | !B | B ^ B | E <= E
	 		B[|True|]sigma = T 		(DTrue)
	 		B[|False|]sigma = F 	(DFalse)
	 		B[|!B|]sigma = ~B[|B|]sigma 	(DNot)
	 		B[|B1 ^ B2|]sigma = B[|B1|]sigma && B[|B2|]sigma 	(DAnd)
	 		B[|E1 <= E2|]sigma = A[|E1|]sigma <= A[|E2|]sigma	(DLeq)

Exemplo:

	B [|!(2<=3) ^ True|]sigma = (DAnd)
	B [|!(2<=3)|]sigma && B [|True|]sigma = (DNot)
	~B[|2<= 3|]sigma && B[|True|]sigma = (DLeq)
 	~(A[|2|]sigma <= 3 A[|3|]sigma) && B[|True]sigma = (Dnum)
 	~(2 <= 3) && B[|True|]sigma = LEQ
 	~(T) && B[|True|]sigma = (DTrue)
 	~(T) && (T) = Lógica Booleana
 		F

C := Skip | x:= E | C;C | IF B then C else C | While B do C
	C[|.|] :: C -> Memória -> Memória
	C[|Skip|]sigma = sigma (Dskip)
	C[|x := E]sigma =  sigma[x|->A[|E|]sigma]	(Datrib)
	C[|C1;C2|]sigma = C[|C2|](C[|C1|]sigma)  	(Dseq)
	C[|IF B then C else C|]sigma = Se B[|B|]sigma entao C[|C1|]sigma senão C[|C2|]sigma  (DIf1)
	Se B entao C1 senao C2 { C1, Se B=T
							 C2, Se B=F}

While -- Tentativa de solução
C[|While B do C |]sigma = Se [|B|]sigma entao C[|C;While B do C|]sigma senao C[|Skip|]sigma
-- NÃO FUNCIONA O MÉTODO ACIMA

Solução Composicional 1 
	C[|While B do C |]sigma = Loop sigma
		onde
			Loop sigma = Se B[|B|]sigma
							entao Loop C[|C|]sigma
							senao sigma

Solução 2 (Ponto Fixo)
	C[|While B do C |]sigma = (Fix w sigma)
		onde
			w g sigma = Se B[|B|]sigma
							entao g C[|C|]sigma
							senao sigma
Fix f = f(Fix f)















