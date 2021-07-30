-- >> Slides do Tópico 1.4 (listas)
-- ficou como exercício implementar a função produtoCartesiano sem usar compreensão de listas

produtoCartesiano :: [Int] -> [Int] -> [(Int,Int)]
produtoCartesiano _ [] = []
produtoCartesiano [] _ = []
produtoCartesiano (x:xs) (y:ys) = [(x,y) ++ produtoCartesiano [x] ys ++ produtoCartesiano xs ys ++ produtoCartesiano

-- também ficou como exercício modelar matrizes com o lista de listas e definir operações sobre elas, por 
-- exemplo soma e multiplicação. 

sumList lista1 lista2 = zipWith (+) lista1 lista2

somaMat :: Num a => [[a]] -> [[a]] -> [[a]]
somaMat l1 l2 = zipWith sumList l1 l2

--Estudo de caso: dada uma matriz L1 = [[1,2,3], [4,5,6]] e uma matriz L2 = [[3,2,1], [6,5,4]] retornar
--uma matriz com a soma de cada elemento equivalente. Resultado: [[4,4,4], [10,10,10]]
