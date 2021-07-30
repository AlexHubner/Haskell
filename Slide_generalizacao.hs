
-- A função de mapeamento MAP - EXEMPLOS:
double :: [Int] -> [Int] --Função que recebe uma lista de inteiros e retorna uma lista com o dobro de cada elemento
double [] = []
double (a:x) = (2*a) : double x

casoTesteDouble1 = double [2,4,6] == [4,8,12]
casoTesteDouble2 = double [11,20,30] == [22,40,60]

resultadoCasoDouble = foldl (&&) True [casoTesteDouble1, casoTesteDouble2]

sqrList :: [Int] -> [Int] --Função que recebe uma lista de inteiros e retorna o quadrado de cada elemento
sqrList [] = []
sqrList (a:x) = (a*a) : sqrList x

casoTesteSqrList1 = sqrList [2,4,6] == [4,16,36]
casoTesteSqrList2 = sqrList [11,20,30] == [121,400,900]

resultadoCasoSqrList = foldl (&&) True [casoTesteSqrList1, casoTesteSqrList2]

times2 :: Int -> Int --Funçao que recebe um valor inteiro e retorna o dobro
times2 n = 2*n

casoTesteTimes2 = times2 11 == 22
casoTesteTimes2b = times2 30 == 60

resultadoCasoTimes2 = foldl (&&) True [casoTesteTimes2, casoTesteTimes2b]

sqr :: Int -> Int --Função que recebe um valor inteiro e retorna o quadrado do mesmo
sqr n = n*n

mapear :: (t -> u) -> [t] -> [u]
mapear f []     = []
mapear f (a:as) = f a : mapear f as

doubleList xs = mapear times2 xs

casoTesteDoubleList = doubleList [6,8,10] == [12,16,20]
casoTesteDoubleList1 = doubleList [12,16,20] == [24,32,40]

resultadoCasoDoubleList = foldl (&&) True [casoTesteDoubleList, casoTesteDoubleList1]

sqrList2 xs = mapear sqr xs

snds :: [(t,u)] -> [u]
snds xs = map snd xs

casoTesteSnds = snds [(2,5)] == [5]
casoTesteSnds2 = snds [(10,30)] == [30] 

resultadoCasoTesteSnd = foldl (&&) True [casoTesteSnds, casoTesteSnds2]

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

casoTesteCrescente1 = eCrescente quadrado 2 == True
casoTesteCrescente2 = eCrescente times2 5 == True

resultadoCasoCrescente = foldl (&&) True [casoTesteCrescente1, casoTesteCrescente2]

-- Defina as seguintes funções sobre listas
-- eleva os itens ao quadrado

-- 1º Implementar uma função que eleve um valor ao quadrado
quadrado :: Int -> Int
quadrado x = x * x

casoTesteQuad1 = quadrado 2 == 4
casoTesteQuad2 = quadrado 120 == 14400

resultadoCasoQuad = foldl (&&) True [casoTesteQuad1, casoTesteQuad2]

--2º Implementar uma função map que aplique a função quadrado a todos os elementos de uma lista
mapQuadrado :: [Int] -> [Int]
mapQuadrado x = map quadrado x

casoTesteMQuad1 = mapQuadrado [10,20,30] == [100,400,900]
casoTesteMQuad2 = mapQuadrado [4,6,8] == [16,36,64]

resultadoCasoMapQuad = foldl (&&) True [casoTesteMQuad1, casoTesteMQuad2]

-- retorna a soma dos quadrados dos itens

somaQuadrados :: [Int] -> Int
somaQuadrados lista = foldr (+) 0 (mapQuadrado lista)

casoTesteSomaQuadrados1 = somaQuadrados [10,20,30] == 1400
casoTesteSomaQuadrados2 = somaQuadrados [6,5,8] == 125

resultadoCasoSomaQuad = foldl (&&) True [casoTesteSomaQuadrados1, casoTesteSomaQuadrados2]

-- Manter na lista todos os itens maiores que zero.
maiorQueZero :: [Int] -> [Int]
maiorQueZero lista = filter f lista 
    where f n = n > 0

casoTesteMaiorQueZero1 = maiorQueZero [0,2,8,0,10] == [2,8,10]
casoTesteMaiorQueZero2 = maiorQueZero [100,23,0,45,69] == [100,23,45,69]

resultadoCasoMaiorQueZero = foldl (&&) True [casoTesteMaiorQueZero1, casoTesteMaiorQueZero2]

fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:as) = f a (fold f as)
