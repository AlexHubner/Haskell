--Slides do Tópico 1.4 (listas)
--Implementar a função produtoCartesiano sem usar compreensão de listas

produtoCartesiano :: [Int] -> [Int] -> [(Int,Int)]
produtoCartesiano xs [] = []
produtoCartesiano [] ys = []
produtoCartesiano (x:xs) ys = map (\y -> (x,y)) ys ++ produtoCartesiano xs ys

casoTestePC = produtoCartesiano [1,2] [2,3] == [(1,3),(1,4),(2,3),(2,4)]
casoTestePC2 = produtoCartesiano [4,4] [8,9] == [(4,8),(4,9),(4,8),(4,9)]

resultadoCasosPC = foldl (&&) True [casoTestePC, casoTestePC2]

--Modelar matrizes com o lista de listas e definir operações sobre elas, por 
--exemplo soma e multiplicação. 

sumList lista1 lista2 = zipWith (+) lista1 lista2

somaMat :: Num a => [[a]] -> [[a]] -> [[a]]
somaMat l1 l2 = zipWith sumList l1 l2

casoTesteSMatriz = somaMat [[1,2,3], [4,5,6]] [[3,2,1], [6,5,4]] == [[4,4,4], [10,10,10]]
casoTesteSMatriz2 = somaMat [[12,23,45], [6,13,9]] [[33,42,61], [26,55,44]] == [[45,65,106],[32,68,53]]

resultadoCasosSM = foldl (&&) True [casoTesteSMatriz, casoTesteSMatriz2]

multList lista1 lista2 = zipWith (*) lista1 lista2

multMat :: Num a => [[a]] -> [[a]] -> [[a]]
multMat l1 l2 = zipWith multList l1 l2
