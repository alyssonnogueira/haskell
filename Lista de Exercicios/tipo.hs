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

-- Exercicio 1
dobraArvore:: Arvore -> Arvore
dobraArvore (Folha n) = Folha (n * 2) 
dobraArvore (No n a1 a2) = No (n*2) (dobraArvore a1) (dobraArvore a2)

-- Exercicio 2
meArvore :: Arvore -> Int
meArvore (Folha n) = n 
meArvore (No n a1 a2) = maior n (maior(meArvore a1) (meArvore a2))

maior :: Int -> Int -> Int
maior a b 
    | a >= b = a
    | otherwise = b

--Exercicio 3 -- Testar
ocorre :: Int -> Arvore -> Bool
ocorre x (Folha n) = n == x
--ocorre x (No n a1 a2) = x == n || ocorre x a1 | ocorre x a2
--Fazer
-- Exercicio 5 
--existe :: Int -> Int
--existe 
-- Exercicio 7
--arvList :: Arvore -> [Int]

-- Exercicio 8
--mapTree :: (Int -> Int) -> Arvore -> Arvore
--mapTree t (Folha n) = 
--mapTree t (No n a1 a2) = 














