somaLista::[Int]->Int
somaLista []=0
somaLista(a:x)= a + somaLista x

dobraLista::[Int]->[Int]
dobraLista []=[]
dobraLista(a:x) = a*2 : dobraLista x

tamanhoLista::[Int]->Int
tamanhoLista []=0
tamanhoLista (a:x) = 1 + tamanhoLista x

produtoLista::[Int]->Int
produtoLista [] = 0
produtoLista [a] = a
produtoLista (a:x) = a*produtoLista x

andLista :: [Bool] -> Bool
andLista [] = False
andLista [a] = a
andLista (a:x) = a && andLista x

concatLista::[[Int]] -> [Int]
concatLista [] = []
concatLista [[a]] = [a]
concatLista (a:x) = a ++ concatLista x

invertLista::[Int]->[Int]
invertLista [] = []
invertLista (a:x) = invertLista x ++ a:[]