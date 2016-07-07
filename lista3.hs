--Exemplo
somaLista::[Int]->Int
somaLista []=0
somaLista(a:x)= a + somaLista x
--Exercicio 1
--1. Implemente a função dobraLista :: [Int] -> [Int] que dobra o valor de todos os elementos de uma lista
dobraLista :: [Int] -> [Int]
dobraLista [] = [] 
dobraLista (a:x) = a*2 : dobraLista x
--2. Implemente a função tamanho :: [Int] -> Int que conta o número de elementos de uma lista
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (a:x) = 1 + tamanho x
--3. Implemente a função produtoLista :: [Int] -> Int que implementa o produto de uma lista de inteiros.
produtoLista :: [Int] -> Int
produtoLista [] = 0
produtoLista [a] = a
produtoLista (a:x) = a * produtoLista x 
--4. Implemente a função andLista :: [Bool] -> Bool que faz um and (&&) entre todos os elementos de uma lista
andLista :: [Bool] -> Bool
andLista [] = False
andLista [a] = a
andLista (a:x) = a && andLista x
--5. Implemente a função concatLista :: [[Int]] ->[Int] que transforma uma lista de lista de inteiros 
--em uma lista única de inteiros. 
--Ex: Hugs> concatLista
--	[1,2,3,2,4,5]
--	[[1,2,3], [2], [4,5]]
concatLista :: [[Int]] ->[Int]
concatLista [] = []
concatLista [[a]] = [a]
concatLista (a:x) = a ++ concatLista x

--6. Implemente a função inverteLista :: [Int] -> [Int] Hugs> inverteLista [1,2,3,4] [4,3,2,1]
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (a:x) = inverteLista x ++ a:[]