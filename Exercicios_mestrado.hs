--1) Implemente funções que satisfaçam a cada um dos requisitos abaixo:
--a) Retorna a diferença entre duas listas. O resultado é uma lista.

diferenca xs ys = [a|a <- xs, not (elem a ys)]

--OU

diferenca2 :: [Int] -> [Int] -> [Int]
diferenca2 [] y = []
diferenca2 (x:xs) y 
        | elem x y = diferenca2 xs y    
        | otherwise = x : (diferenca2 xs y)

-- Caso Teste 1 = L1 [1,2,3,4] L2 [2,3,4,5] = [1,5]
-- Caso Teste 2 = L1 [20,33,41,70] L2 [21,33,39,70,85] = [21,39,41,85]

--b) Retorna a interseção entre duas listas. O resultado é uma lista.

inter xs ys = [a|a <- xs, elem a ys]

--OU
inter2 :: [Int] -> [Int] -> [Int]
inter2 xs [] = xs
inter2 [] ys = ys
inter2 (x:xs) ys
      | x `elem` ys = x: inter2 xs ys
      | otherwise = inter2 xs ys

-- Caso Teste 1 = L1 [1,2,3,4] L2 [2,3,4,5] = [2,3,4]
-- Caso Teste 2 = L1 [20,33,41,70] L2 [21,33,39,70,85] = [33,70]

--c) Retorna a união entre duas listas (pode haver repetição de elementos). O resultado é uma lista.

uniao x y = x ++ y


-- Caso Teste 1 = L1 [1,2,3,4] L2 [2,3,4,5] = [1,2,3,4,2,3,4,5]
-- Caso Teste 2 = L1 [20,33,41,70] L2 [21,33,39,70,85] = [20,33,41,70,21,33,39,70,85]

--d) Retorna a união entre duas listas (não há repetições de elementos). O resultado é uma lista.

listaSemRepeticao x y = tr (uniao x y)
                        where tr [] = []
                              tr (x:xs)
                                    | elem x xs = tr xs
                                    | otherwise = x : tr xs


-- Caso Teste 1 = L1 [1,2,3,4] L2 [2,3,4,5] = [1,2,3,4,5]
-- Caso Teste 2 = L1 [20,33,41,70] L2 [21,33,39,70,85] = [20,41,21,33,39,70,85]

--e) Retorna o último elemento de uma lista.

ultimo xs = last xs

-- Caso Teste 1 = L1 [1,2,3,4] = 4
-- Caso Teste 2 = L1 [20,33,41,70] = 70

--f) Retorna o n-ésimo elemento de uma lista.
nelemento :: Int -> [a] -> a
nelemento _ [] = error "Lista Vazia"
nelemento e (x:xs)
    | e <= 0 = x
    | otherwise = nelemento (e - 1) xs


-- Caso Teste 1 = L1 [1,2,3,4] elemento 3 = 4
-- Caso Teste 2 = L1 [20,33,41,70] elemento 2 = 41

--g) Inverte uma lista.

inverter xs = reverse xs

--    ESTUDO DE CASO: Seja uma lista x = [1,2,3,4], como ficaria essa mesma lista de forma invertida?
-- RESPOSTA ESPERADA: [4,3,3,1]

-- Caso Teste 1 = L1 [1,2,3,4] = [4,3,2,1]
-- Caso Teste 2 = L1 [20,33,41,70] = [70,41,33,20]

--h) Ordena uma lista em ordem descrescente, removendo as eventuais repetições de elementos.

ordemDec :: Ord a => [a] -> [a]
ordemDec [] = []
ordemDec (x:xs) = ordemDec ma ++ [x] ++ ordemDec mn
                    where ma = [e | e <- xs, e > x]
                          mn = [e | e <- xs, e < x]

-- Caso Teste 1 = L1 [1,2,3,4,4,5] = [5,4,3,2,1]
-- Caso Teste 2 = L1 [1,3,2,4,2] = [4,3,2,1]

--i) Retorna um booleano indicando se uma lista de inteiros é decrescente ou não. Proponha 3  soluções: 

-- Caso Teste 1 = L1 [1,2,3,4,4,5] = False
-- Caso Teste 2 = L1  [4,3,2,1] = True

-- a) usando sort;

testaDec x = x == (ordemDec x)


--b) usando apenas recursão:

testaDec2 :: [Int] -> Bool
testaDec2 [] = False
testaDec2 [a] = True
testaDec2 (a:b:xs) = a >= b && testaDec2 (b:xs)


--c) usando fold, map e zip.
testaDec3 x = fold (&&) (map (\(a,b) -> a>=b) (zip x (tail x)) )

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
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

--3) Crie a função foldTree, que recebe uma função e uma árvore polimórfica binária como
--parâmetros e retorna o valor resultante de acumular a aplicação dessa função por todos os nós da
--árvore.

foldTree f NilT = 0
foldTree f (Node n l r) = f n ( f (foldTree f l) (foldTree f r) )

data Tree t = NilT |
               Node t (Tree t) (Tree t)


--4) Defina uma função que some os elementos de uma árvore binária que armazena inteiros em seus
--nós. Resolva o exerício de duas formas diferentes:

--a) usando a função foldTree definida acima;
somaArvore arvore = foldTree (+) arvore

--b) sem usar a função foldTree
somaArvore2 NilT = 0
somaArvore2 (Node n l r) =  n + (somaArvore2 l) + (somaArvore2 r)

--5) Sendo a função addNum abaixo
--addNum :: Int -> (Int -> Int)
--addNum n = h
 --where h m = n + m
--redefina-a usando aplicação parcial de função.

addNum n = (n+)
