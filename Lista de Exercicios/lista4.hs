--1. Defina a função membro :: [Int] -> Int -> Bool
--que retorna um booleando que diz se o inteiro esta na lista
membro :: [Int] -> Int -> Bool
membro [] b = False
membro(a:x) b
	| a == b = True
	| a /= b = membro x b  

--2. Implemente a função membroNum :: [Int] -> Int -> Int
--que conta o número de vezes que o inteiro aparece na lista
membroNum :: [Int] -> Int -> Int
membroNum [] b = 0
membroNum(a:x) b
	| a == b = 1 + membroNum x b
	| a /= b = membroNum x b
--3. Defina a função membro usando a função membroNum
membro2 :: [Int] -> Int -> Bool
membro2 x b
	| (membroNum x b) == 0 = False
	| (membroNum x b) > 0 = True
	| otherwise = False

--4. Implemente a função unico :: [Int] -> [Int] que retorna uma lista com os números que aparecem apenas uma vez na
--lista argumento. Ex:
--		Hugs> unico [2,4,1,4,1,3]
--		[2,3]
--A função memberNum deve ser usada na definição de unico.
unico :: [Int] -> [Int]
unico [] = []
unico (a:x)
	| membroNum x a > 0 = unico x
	| membroNum x a == 0 = a : unico x

--5. Se a lista argumento para membro está ordenada, não é necessário percorrer toda a lista para saber se o elemento 
--está presente na lista. Implemente uma nova definição de membro, que use iSort.
--iSort :: [Int] -> [Int] 
