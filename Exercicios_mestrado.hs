--1) Implemente funções que satisfaçam a cada um dos requisitos abaixo:
--a) Retorna a diferença entre duas listas. O resultado é uma lista.

diferenca xs ys = [a|a <- xs++ys, notElem a ys]

--OU

diferenca2 :: [Int] -> [Int] -> [Int]
diferenca2 [] y = []
diferenca2 (x:xs) y 
        | elem x y = diferenca2 xs y    
        | otherwise = x : (diferenca2 xs y)

--b) Retorna a interseção entre duas listas. O resultado é uma lista.

inter xs ys = [a|a <- xs, elem a ys]

--OU
inter2 :: [Int] -> [Int] -> [Int]
inter2 xs [] = xs
inter2 [] ys = ys
inter2 (x:xs) ys
      | x `elem` ys = x: inter2 xs ys
      | otherwise = inter2 xs ys



--c) Retorna a união entre duas listas (pode haver repetição de elementos). O resultado é uma lista.

uniao x y = x ++ y

--d) Retorna a união entre duas listas (não há repetições de elementos). O resultado é uma lista.

listaSemRepeticao x y = tr (uniao x y)
                        where tr [] = []
                              tr (x:xs)
                                    | elem x xs = tr xs
                                    | otherwise = x : tr xs

--e) Retorna o último elemento de uma lista.

ultimo xs = last xs

--f) Retorna o n-ésimo elemento de uma lista.
nelemento :: Int -> [a] -> a
nelemento _ [] = error "Lista Vazia"
nelemento e (x:xs)
    | e <= 0 = x
    | otherwise = nelemento (e - 1) xs

--g) Inverte uma lista.

inverter xs = reverse xs

--h) Ordena uma lista em ordem descrescente, removendo as eventuais repetições de elementos.

ordemDec :: Ord a => [a] -> [a]
ordemDec [] = []
ordemDec (x:xs) = ordemDec (ma ++ [x] ++ ordemDec mn)
                    where ma = [e | e <- xs, e > x]
                          mn = [e | e <- xs, e <= x]

--i) Retorna um booleano indicando se uma lista de inteiros é decrescente ou não. Proponha 3  soluções: 

-- a) usando sort;

testaDec x = x == (ordemDec x)


--b) usando apenas recursão:

testaDec2 :: [Int] -> Bool
testaDec2 [] = False
testaDec2 [a] = True
testaDec2 (a:b:xs) = a >= b && testaDec2 (b:xs)


 --c) usando fold, map e zip.
 

 
