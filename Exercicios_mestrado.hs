--1) Implemente funções que satisfaçam a cada um dos requisitos abaixo:
--a) Retorna a diferença entre duas listas. O resultado é uma lista.

diferenca xs ys = [a|a <- xs++ys, notElem a ys]

--OU

diferenca2 :: [Int] -> [Int] -> [Int]
diferenca2 [] y = []
diferenca2 (x:xs) y 
        | elem x y = diferenca2 xs y    
        | otherwise = x : (diferenca2 xs y)

--    ESTUDO DE CASO: Seja uma lista x = [1,2,3,4] e uma lista y = [2,3,4,5], como seria uma lista contendo somente os elementos diferentes
-- entre as duas listas? RESPOSTA ESPERADA: [1,5]

--b) Retorna a interseção entre duas listas. O resultado é uma lista.

inter xs ys = [a|a <- xs, elem a ys]

--OU
inter2 :: [Int] -> [Int] -> [Int]
inter2 xs [] = xs
inter2 [] ys = ys
inter2 (x:xs) ys
      | x `elem` ys = x: inter2 xs ys
      | otherwise = inter2 xs ys

--    ESTUDO DE CASO: Seja uma lista x = [1,2,3,4] e uma lista y = [2,3,4,5], como seria uma lista contendo a interseção
--(elementos iguais) entre as duas listas? RESPOSTA ESPERADA: [2,3,4]

--c) Retorna a união entre duas listas (pode haver repetição de elementos). O resultado é uma lista.

uniao x y = x ++ y

--    ESTUDO DE CASO:  Seja uma lista x = [1,2,3,4] e uma lista y = [2,3,4,5], como seria uma lista contendo os elementos de
-- ambas as listas? RESPOSTA ESPERADA:

--d) Retorna a união entre duas listas (não há repetições de elementos). O resultado é uma lista.

listaSemRepeticao x y = tr (uniao x y)
                        where tr [] = []
                              tr (x:xs)
                                    | elem x xs = tr xs
                                    | otherwise = x : tr xs

--    ESTUDO DE CASO:  Seja uma lista x = [1,2,3,4] e uma lista y = [2,3,4,5], como seria uma lista contendo os elementos de
-- ambas as listas, mas sem a repetição dos elementos? RESPOSTA ESPERADA:

--e) Retorna o último elemento de uma lista.

ultimo xs = last xs

--    ESTUDO DE CASO:  Seja uma lista x = [1,2,3,4], qual é o último elemento da lista?  RESPOSTA ESPERADA: 4

--f) Retorna o n-ésimo elemento de uma lista.
nelemento :: Int -> [a] -> a
nelemento _ [] = error "Lista Vazia"
nelemento e (x:xs)
    | e <= 0 = x
    | otherwise = nelemento (e - 1) xs

--    ESTUDO DE CASO: Seja uma lista x = [1,2,3,4], qual seria o terceiro elemento dessa lista? RESPOSTA ESPERADA: 3

--g) Inverte uma lista.

inverter xs = reverse xs

--    ESTUDO DE CASO: Seja uma lista x = [1,2,3,4], como ficaria essa mesma lista de forma invertida?
-- RESPOSTA ESPERADA: [4,3,3,1]

--h) Ordena uma lista em ordem descrescente, removendo as eventuais repetições de elementos.

ordemDec :: Ord a => [a] -> [a]
ordemDec [] = []
ordemDec (x:xs) = ordemDec (ma ++ [x] ++ ordemDec mn)
                    where ma = [e | e <- xs, e > x]
                          mn = [e | e <- xs, e <= x]

--    ESTUDO DE CASO: Dada uma lista x = [1,3,2,4,2] como fica essa lista em ordem decrescente e sem repetições em seus elementos?
-- RESPOSTA ESPERADA: [4,3,2,1]

--i) Retorna um booleano indicando se uma lista de inteiros é decrescente ou não. Proponha 3  soluções: 
--    ESTUDO DE CASO: Seja uma lista x = [1,2,3,4] ela está ordenada de forma decrescente?
-- RESPOSTA ESPERADA: Falso

-- a) usando sort;

testaDec x = x == (ordemDec x)


--b) usando apenas recursão:

testaDec2 :: [Int] -> Bool
testaDec2 [] = False
testaDec2 [a] = True
testaDec2 (a:b:xs) = a >= b && testaDec2 (b:xs)


--c) usando fold, map e zip.
testaDec3 x = foldr (&&) (map (\(a,b) -> a>=b) (zip x)

--Entendendo:
--foldr vai aplicar a função (&&) nas funções map e zip
--map vai testar se um elemento é maior do que o outro
--zip vai agrupando os elementos em pares
