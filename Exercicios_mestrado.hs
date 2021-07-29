--1) Implemente funções que satisfaçam a cada um dos requisitos abaixo:
--a) Retorna a diferença entre duas listas. O resultado é uma lista.

diferenca xs ys = [a|a <- xs, not (elem a ys)]

--OU

diferenca2 :: [Int] -> [Int] -> [Int]
diferenca2 [] y = []
diferenca2 (x:xs) y 
        | elem x y = diferenca2 xs y    
        | otherwise = x : (diferenca2 xs y)

casoTesteQ1a = diferenca2 [1,2,3,4]  [2,3,4,5] == [1,5]
casoTesteQ1a2 = diferenca2 [20,33,41,70] [21,33,39,70,85] == [20,21,39,41,85]

resultadoCTQ1a = foldl (&&) True [casoTesteQ1a, casoTesteQ1a2]

--b) Retorna a interseção entre duas listas. O resultado é uma lista.

inter xs ys = [a|a <- xs, elem a ys]

--OU
inter2 :: [Int] -> [Int] -> [Int]
inter2 xs [] = xs
inter2 [] ys = ys
inter2 (x:xs) ys
      | x `elem` ys = x: inter2 xs ys
      | otherwise = inter2 xs ys

casoTesteQ1b = inter2 [1,2,3] [2,3,4] == [2,3]
casoTesteQ1b2 = inter2 [100,200,300] [300,400] == [300]

resultadoCTQ1b = foldl (&&) True [casoTesteQ1b, casoTesteQ1b2]



--c) Retorna a união entre duas listas (pode haver repetição de elementos). O resultado é uma lista.

uniao x y = x ++ y


casoTesteQ1c = uniao [1,2,3,4] [2,3,4,5] == [1,2,3,4,2,3,4,5]
casoTesteQ1c2 = uniao [20,33,41,70] [21,33,39,70,85] == [20,33,41,70,21,33,39,70,85]

resultadoCTQ1c = foldl (&&) True [casoTesteQ1c, casoTesteQ1c2]

--d) Retorna a união entre duas listas (não há repetições de elementos). O resultado é uma lista.

listaSemRepeticao x y = tr (uniao x y)
                        where tr [] = []
                              tr (x:xs)
                                    | elem x xs = tr xs
                                    | otherwise = x : tr xs


casoTesteQ1d = listaSemRepeticao [1,2,3,4] [2,3,4,5] == [1,2,3,4,5]
casoTesteQ1d2 = listaSemRepeticao [20,33,41,70] [21,33,39,70,85] == [20,41,21,33,39,70,85]

resultadoCTQ1d = foldl (&&) True [casoTesteQ1d, casoTesteQ1d2]

--e) Retorna o último elemento de uma lista.

ultimo xs = last xs

casoTesteQ1e = ultimo [1,2,3,4] == 4
casoTesteQ1e2 = ultimo [20,33,41,70] == 70

resultadoCTQ1e = foldl (&&) True [casoTesteQ1e, casoTesteQ1e2]

--f) Retorna o n-ésimo elemento de uma lista.
nelemento :: Int -> [a] -> a
nelemento _ [] = error "Lista Vazia"
nelemento e (x:xs)
    | e <= 0 = x
    | otherwise = nelemento (e - 1) xs


casoTesteQ1f = nelemento 3 [1,2,3,4] == 4
casoTesteQ1f2 = nelemento 2 [20,33,41,70] == 41

resultadoCTQ1f = foldl (&&) True [casoTesteQ1f, casoTesteQ1f2]

--g) Inverte uma lista.

inverter xs = reverse xs

casoTesteQ1g = inverter [1,2,3,4] == [4,3,2,1]
casoTesteQ1g2 = inverter [20,33,41,70] == [70,41,33,20]

resultadoCTQ1g = foldl (&&) True [casoTesteQ1g, casoTesteQ1g2]

--h) Ordena uma lista em ordem descrescente, removendo as eventuais repetições de elementos.

ordemDec :: Ord a => [a] -> [a]
ordemDec [] = []
ordemDec (x:xs) = ordemDec ma ++ [x] ++ ordemDec mn
                    where ma = [e | e <- xs, e > x]
                          mn = [e | e <- xs, e < x]

casoTesteQ1h = ordemDec [1,2,3,4,4,5] == [5,4,3,2,1]
casoTesteQ1h2 = ordemDec [1,3,2,4,2] == [4,3,2,1]

resultadoCTQ1h = foldl (&&) True [casoTesteQ1h, casoTesteQ1h2]

--i) Retorna um booleano indicando se uma lista de inteiros é decrescente ou não. Proponha 3  soluções: 

-- Caso Teste 1 = L1 [1,2,3,4,4,5] = False
-- Caso Teste 2 = L1  [4,3,2,1] = True

-- a) usando sort;

testaDec x = x == ordemDec x

casoTesteQ1ia = testaDec [5,4,3,2,1] == True
casoTesteQ1ia2 = testaDec [1,2,3,4,5] == False

resultadoCTQ1ia = foldl (&&) True [casoTesteQ1ia, casoTesteQ1ia2]

--b) usando apenas recursão:

testaDec2 :: [Int] -> Bool
testaDec2 [] = False
testaDec2 [a] = True
testaDec2 (a:b:xs) = a >= b && testaDec2 (b:xs)

casoTesteQ1ib = testaDec2 [5,4,3,2,1] == True
casoTesteQ1ib2 = testaDec2 [1,2,3,4,5] == False

resultadoCTQ1ib = foldl (&&) True [casoTesteQ1ib, casoTesteQ1ib2]

--c) usando fold, map e zip.
testaDec3 x = fold (&&) (map (\(a,b) -> a>=b) (zip x (tail x)) )

casoTesteQ1ic = testaDec3 [5,4,3,2,1] == True
casoTesteQ1ic2 = testaDec3 [1,2,3,4,5] == False

resultadoCTQ1ic = foldl (&&) True [casoTesteQ1ic, casoTesteQ1ic2]

--Entendendo:
--fold vai aplicar a função (&&) nas funções map e zip
--map vai testar se um elemento é maior do que o outro
--zip vai agrupando os elementos em pares

fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:as) = f a (fold f as)

--2) Com relação ao material de Tipos Algébricos (última aula), estenda o tipo Expr para poder
--também representar multiplicação. Altere também a definição da função de avaliação eval

data Expr = Lit Int|
            Add Expr Expr |
            Sub Expr Expr |
            Mul Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

--3) Crie a função foldTree, que recebe uma função e uma árvore polimórfica binária como
--parâmetros e retorna o valor resultante de acumular a aplicação dessa função por todos os nós da
--árvore.

data Tree = NilT | Node Int Tree Tree

arvore :: Tree
arvore = (Node 1 (Node 2 (Node 4 NilT NilT) (Node 5 NilT NilT)) (Node 3 (Node 6 NilT NilT) NilT))

arvore2 :: Tree
arvore2 = (Node 34 (Node 5 NilT NilT) NilT)

foldTree f NilT = 0
foldTree f (Node n l r) = f n ( f (foldTree f l) (foldTree f r) )

casoTesteQ3 = foldTree (+) (Node 1 (Node 2 (Node 4 NilT NilT) (Node 5 NilT NilT)) (Node 3 (Node 6 NilT NilT) NilT)) == 21
casoTesteQ3_2 = foldTree (+) (Node 34 (Node 5 NilT NilT) NilT) == 39

--4) Defina uma função que some os elementos de uma árvore binária que armazena inteiros em seus
--nós. Resolva o exerício de duas formas diferentes:

--a) usando a função foldTree definida acima;
somaArvore = foldTree (+)

casoTesteQ4a = somaArvore (Node 1 (Node 2 (Node 4 NilT NilT) (Node 5 NilT NilT)) (Node 3 (Node 6 NilT NilT) NilT)) == 21
casoTesteQ4a2 = somaArvore (Node 34 (Node 5 NilT NilT) NilT) == 39

--b) sem usar a função foldTree
somaArvore2 NilT = 0
somaArvore2 (Node n l r) =  n + somaArvore2 l + somaArvore2 r

casoTesteQ4b = somaArvore2 (Node 1 (Node 2 (Node 4 NilT NilT) (Node 5 NilT NilT)) (Node 3 (Node 6 NilT NilT) NilT)) == 21
casoTesteQ4b2 = somaArvore2 (Node 34 (Node 5 NilT NilT) NilT) == 39

--5) Sendo a função addNum abaixo
--addNum :: Int -> (Int -> Int)
--addNum n = h
 --where h m = n + m
--redefina-a usando aplicação parcial de função.

addNum n = (n+)

casoTesteQ5 = addNum 10 20  == 30
casoTesteQ5_2 = addNum 5 6 == 11
