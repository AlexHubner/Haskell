-- >> Slides do Tópico 1.4 (listas)
-- ficou como exercício implementar a função produtoCartesiano sem usar compreensão de listas

produtoCartesiano :: [Int] -> [Int] -> [(Int,Int)]
produtoCartesiano x y = zip x y

-- também ficou como exercício modelar matrizes com o lista de listas e definir operações sobre elas, por 
-- exemplo soma e multiplicação.  

mat = array ((1,1), (2,3)) [((1,1),4), ((1,2)0), ((1,3), 3), ((2,1),5), ((2,2),1) ((2,3),4)]

somaMat :: (Int a, Int b) => Array (a,a) b -> Array (a,a) b -> Array (a,a) b