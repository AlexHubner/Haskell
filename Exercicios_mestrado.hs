--1) Implemente funções que satisfaçam a cada um dos requisitos abaixo:
--a) Retorna a diferença entre duas listas. O resultado é uma lista.

diferenca xs ys = [a|a <- xs++ys, notElem a ys]

--b) Retorna a interseção entre duas listas. O resultado é uma lista.

inter xs ys = [a|a <- xs, elem a ys]

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



 --c) usando fold, map e zip.
 

 