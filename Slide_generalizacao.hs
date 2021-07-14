
-- A função de mapeamento MAP - EXEMPLOS:
double :: [Int] -> [Int] --Função que recebe uma lista de inteiros e retorna uma lista com o dobro de cada elemento
double [] = []
double (a:x) = (2*a) : double x

sqrList :: [Int] -> [Int] --Função que recebe uma lista de inteiros e retorna o quadrado de cada elemento
sqrList [] = []
sqrList (a:x) = (a*a) : sqrList x

times2 :: Int -> Int --Funçao que recebe um valor inteiro e retorna o dobro
times2 n = 2*n

sqr :: Int -> Int --Função que recebe um valor inteiro e retorna o quadrado do mesmo
sqr n = n*n

mapear :: (t -> u) -> [t] -> [u]
mapear f []     = []
mapear f (a:as) = f a : mapear f as

doubleList xs = mapear times2 xs

sqrList2 xs = mapear sqr xs

snds :: [(t,u)] -> [u]
snds xs = map snd xs

--  EXEMPLO: ANÁLISE DE VENDAS

total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f n = total f (n-1) + f n

totalSales n = totalSales n

sumSquares :: Int -> Int
sumSquares n = total sqr n

-- OUTROS EXEMPLOS
-- Use a função maxFun para implementar a função que retorna o maior número de vendas
--de uma semana de 0 a n semanas

--1º Passo: Criar uma função que recebendo dois valores retorna o maior
maior :: Int -> Int -> Int
maior v1 v2
    |v1 > v2 = v1
    |otherwise = v2

--2º Passo: Implementar a função maxFun (slide 10)
maxFun :: (Int -> Int) -> Int -> Int
maxFun f 0 = f 0
maxFun f n = maior (maxFun f (n-1)) (f n)

--3º Passo: Implementar as funções de Vendas
vendas :: Int -> Int 
vendas x = 10 * x

maiorVenda :: Int -> Int
maiorVenda x = if x == 0
    then vendas 0
    else if x > 0
        then maior (maiorVenda (x-1)) (vendas x)
        else 0



-- Dada uma função, verificar se ela é crescente em um intervalo de 0 a n
eCrescente :: (Int -> Int) -> Int -> Bool
eCrescente f 0 = True
eCrescente f x = if (x > 0) && (f x > f (x-1)) then eCrescente f (x-1) --verifica valor > 0 se a função aplicada ao valor é maior que o próximo
                else False

-- Defina as seguintes funções sobre listas
-- eleva os itens ao quadrado

-- 1º Implementar uma função que eleve um valor ao quadrado
quadrado :: Int -> Int
quadrado x = x * x

--2º Implementar uma função map que aplique a função quadrado a todos os elementos de uma lista
mapQuadrado :: (Int -> Int) -> [Int] -> [Int]
mapQuadrado _ [] = []
mapQuadrado f (x:xs) = (f x) : (mapQuadrado f xs) 
--Estudo de Caso: dada uma lista com os valores [10, 20, 30] retornar uma lista com o dobro de cada elemento [100,400,900]

-- retorna a soma dos quadrados dos itens

--somaQuadrados :: [Int] -> Int
--somaQuadrados (lista) = foldl1 (+) 0 (mapQuadrado lista)
--Estudo de Caso: Usando a mesma lista do caso anterior retornar o valor da soma dos quadrados dos elementos

-- Manter na lista todos os itens maiores que zero.
maiorQueZero :: [Int] -> [Int]
maiorQueZero lista = filter f lista 
    where f n = n > 0

--Estudo de caso: dada uma lista [0,2,8,0,10] retornar uma lista somente com valores maiores que zero