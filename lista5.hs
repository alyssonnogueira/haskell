data Temperatura = Frio | Calor
	deriving(Eq,Show)

data Estacao = Verao | Outono | Inverno| Primavera
	deriving(Eq, Show)

tempo::Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio                    

data Funcionario = Gerente String Int
				| Tecnico String Int
	deriving(Eq,Show)

criaGerente :: String -> Int -> Funcionario
criaGerente nome idade = Gerente nome idade

getNome :: Funcionario -> String
getNome (Gerente nome idade) = nome
getNome (Tecnico nome idade) = nome

andre :: Funcionario
andre = Tecnico "Andre Du Bois" 38

data Forma = Circulo Float | Retangulo Float Float
			| Quadrado Float
	deriving(Eq, Show)

redondo :: Forma -> Bool
redondo (Circulo _ ) = True
redondo _ = False

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a
area (Quadrado l) = l * l

data Arvore = Folha Int | No Int Arvore Arvore
	deriving(Eq, Show)

arv1::Arvore
arv1 = No 3 (Folha 4) (Folha 5)

arv2:: Arvore
arv2 = No 2 ( No 9(Folha 3) (Folha 4))
		 (No 6 (Folha 5) (No 1 (Folha 3) (Folha 7)))

arv3::Arvore
arv3 = No 22 arv1 arv2

somaArvore :: Arvore -> Int
somaArvore (Folha n) = n
somaArvore (No n a1 a2) = n + somaArvore a1 + somaArvore a2

--1.Defina uma função que multiplique por 2 os inteiros em uma árvore.
dobraArvore:: Arvore -> Arvore
dobraArvore (Folha n) = Folha (n*2)
dobraArvore (No n a1 a2) = No (n*2) (dobraArvore a1) (dobraArvore a2) 

--2.Defina uma função que ache o maior elemento de uma árvore
maiorEArvore::Arvore -> Int
maiorEArvore (Folha n) = n 
maiorEArvore (No n a1 a2) = maior n (maior(maiorEArvore a1) (maiorEArvore a2))

maior :: Int -> Int -> Int
maior a b 
	| a >= b = a
	| a <= b = b
	| otherwise = 0
--3.Defina a função que diz se um inteiro ocorre dentro de uma árvore
ocorreEArvore::Arvore -> Int -> Bool
maiorEArvore (Folha n) x
	| x == n = True
	| x \= n = False  
maiorEArvore (No n a1 a2) x = maior n (maior(maiorEArvore a1) (maiorEArvore a2))
	| n == x = True
	| n \= x  
--4.Defina uma função que ache o mair inteiro dentro de uma árvore

--5.Defina uma função que diz quantas vezes um inteiro ocorre dentro de uma árvore

--6. Uma árvore refletida é uma árvore com seus ramos esquerdos e direitos trocados. Defina uma função refleteArvore

--7. Defina uma função que transfore uma árvore em uma lista

--8. Defina a função mapTree que aplica uma função a todos os inteiros de todos os nós de uma árvore.
--mapTree :: (Arvore -> Arvore) -> Arvore -> Arvore

--9. Defina uma lista como sendo um tipo algébrico recursivo. Defina as funções tamanho (que conta o número de elementos de uma --lista) e a função map que operem no tipo lista definido.