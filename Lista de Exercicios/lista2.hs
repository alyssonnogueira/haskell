--Exercicio 1
--1. Defina uma função max :: Int -> Int -> Int que retorna o maior entre dois números.
max1 :: Int -> Int -> Int
max1 a b
	| a >= b = a
	| b > a = b
	| otherwise = 0 

--2. Usando a funçao max, defina uma função maiorVenda que recebe um argumento numérico n, 
--e calcule a maior venda em uma semana entre 0 e n.
maiorVenda :: Int -> Int
maiorVenda a 
	| a == 0 = vendaSemanal a 
	| a > 0 = max1 (vendaSemanal a) (maiorVenda (a-1))
	| otherwise = 0

vendaSemanal :: Int -> Int 
vendaSemanal a
	| a == 0 = 10
	| a == 1 = 0
	| a == 2 = 20
	| otherwise = -1	

--3. Defina uma função maxVenda que recebe um argumento numérico n, e calcule a semana, entre 0 e n, 
--que teve o maior número de vendas. Essa função deve usar maiorVenda em sua definição
maxVenda :: Int -> Int
maxVenda a = maiorVenda a

--4. Defina uma função zeroVendas que recebe um argumento numérico n, e que calcula qual das semanas entre 0 e n 
--teve vendas igual a 0. Se nenhuma semana teve vendas igual a 0 a função retorna -1
zeroVendas :: Int -> Int
zeroVendas a
	|a >= 0 = if ((vendaSemanal a) == 0) then a else (if ((zeroVendas (a-1)) == 0) then a else -1) 
	|otherwise = -1
	
--5. Usando a definição anterior como guia, defina uma função que receba um valor s e uma semana n, 
--e devolva qual das semanas entre 0 e n teve vendas iguais a s
vendasIgual :: Int -> Int -> Int
vendasIgual s n
	|n >= 0 = if ((vendaSemanal n) == s) then n else (if ((vendasIgual s (n-1)) == s) then n else -1) 
	|otherwise = -1


--6. Como você usaria a função anterior para definir a função zeroVendas. Hã? 

--7. As funções definidas até agora operam em um periodo entre 0 e n. Defina versões alternativas dessas funções que 
--trabalhem em um periodo entre m e n, assumindo que n sempre é maior que m.


--8. O fatorial de um número positivo n é 1 * 2 * ... * (n-1) * n Defina uma função fatorial em Haskell
fat :: Int -> Int
fat n
	| n == 0 = 1
	| n == 1 = 1
	| n > 1 = n * (fat (n-1))
	| otherwise = 1 

--9. Defina uma função que receba dois argumentos m e n e retorne o produto m * (m+1) * ... * (n-1) * n
prod :: Int -> Int -> Int
prod m n
	| n == m = n
	| n /= m = m * (prod (m+1) n) * (prod m (n-1)) * n
	| otherwise = 1

--10. Considere a sequência fibonacci de números: 0, 1, 1, 2, 3, 5, ... cujos dois primeiros valores são 0 e 1, 
--e os valores seguintes são sempre a soma dos dois valores anteriores (0+1=1, 1+1=2, 1+2=3, ...)
--Escreva em Haskell a função fib sendo que fib n devolve o número que esta na posição n da sequência fibonacci
fibo :: Int -> Int
fibo n 
	| n <= 0 = 1
	| n <= 1 = 1
	| n > 1 = (fibo (n-1)) + (fibo (n-2))