-- Exercicio 1
osQuatroSaoIguais::Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d
	| (a == b) && (b == c) && (c == d) = True
	| otherwise = False

-- Exercicio 2
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c = (somaIguais a b) + (somaIguais b c) + (somaIguais a c)

somaIguais :: Int -> Int -> Int	 
somaIguais x y
	| (x == y) = 1
	| otherwise = 0

-- Exercicio 3
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c
	| (a /= b) && (b /= c) && (a /= c) = True
	| otherwise = False

-- Exercicio 4
resultT :: Bool
resultT = todosDiferentes 1 2 3
resultF :: Bool
resultF = todosDiferentes 1 1 1

-- Exercicio 5
todosDiferentes2 :: Int -> Int -> Int -> Bool
todosDiferentes2 n m p = ( ( n/=m ) && ( m/=p ) )
-- Funciona mas a função não avalia se n e p são diferetes,
-- portanto nem todos os resultados estarão corretos.

-- Exercicio 6
todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c
	| ((a == b) && (b == c)) = True
	| otherwise = False

-- Exercicio 7
quantosSaoIguais2 :: Int -> Int -> Int -> Int
quantosSaoIguais2 a b c 
	| (todosDiferentes a b c) == True = 0
	| (todosIguais a b c) == True = 3
	| otherwise = 2

-- Exercicio 8
elevadoDois :: Int -> Int
elevadoDois a = a*a

-- Exercicio 9
elevadoQuatro :: Int -> Int
elevadoQuatro a = (elevadoDois a)*(elevadoDois a)

-- Exercicio 10
vendas :: Int -> Int
vendas a
	| a == 0 = 4
	| a == 1 = 6
	| a == 2 = 5
	| otherwise = 0

vendaTotal :: Int -> Int
vendaTotal n
	| n == 0 = vendas 0
	| n > 0 = (vendas n) + (vendaTotal (n-1))
	| n < 0 = 0






