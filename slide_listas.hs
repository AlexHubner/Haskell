-- Somar elementos de uma lista:
sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as
-- Estudo de Caso: Dada uma lista [1..10] apresentar a soma dos elementos da mesma. Resultado: 55

-- Dobrar os elementos de uma lista:
dobrar :: [Int] -> [Int]
dobrar [] = []
dobrar (x:xs) = 2 * x : dobrar xs
-- Estudo de Caso: Dada uma lista [1,2,3,4,5] apresentar o dobro do valor de cada elemento da mesma. Resultado: [1,4,6,8,10]

-- Perterncer: checar se um elemento está na lista:
pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence a (x:xs)
           | a == x  =  True
           | otherwise = pertence a xs

-- Estudo de Caso: Dada uma lista [1,2,3,4,5] verificar se o valor 3 é um elemento da mesma. Resultado: True

-- Filtragem: apenas os números de uma string
--import Data.Char

--digitos :: String -> String
--digitos [] = []
--digitos (a:as)
--            | isDigit a = a : digitos as
--            | otherwise = digitos as


-- Soma de uma lista de pares
soma :: Int -> Int -> Int
soma a b = a+b

somarPares :: [(Int,Int)] -> [Int]
somarPares [] = []
somarPares ((a,b):xs) = (soma a b) : (somarPares xs)

-- Comprimento
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (a:as) = 1 + tamanho as

-- Estudo de Caso: Dada uma lista [1,2,3,4,5] retornar o tamanho da mesma. Resultado: 5


-- COMPREENSÕES DE LISTAS

dobrarListas x = [2*a|a <- x]

-- Estudo de Caso: Dada uma lista [1,2,3,4,5] apresentar o dobro do valor de cada elemento da mesma. Resultado: [1,4,6,8,10]

somarPares2 :: [(Int, Int)] -> [Int]
somarPares2 x = [a+b | (a,b) <- x]

--Exercícios do Slide 17 (funções borrowed e numBorrowed), Slide 18 (returnLoan), e Slide 20 (todas as funções)
type Person = String
type Book = String
type Database = [(Person, Book)]

exampleBase = [("Alice", "Postman Pat"), ("Anna", "All Alone"), ("Alice","Spot"), ("Rory", "Postman Pat")]
books :: Database -> Person -> [Book] -- função que recebe o banco de dados e o nome da pessoa e retorna o livro
books [] _ = []
books ((pes,livro):cauda) pessoa
        |(pessoa == pes) = livro : (books cauda pessoa)
        |otherwise = books cauda pessoa
-- Estudo de caso: tendo como entradas o nome do BD (exempleBase) e o nome de uma pessoa ("Alice") qual será o livro retornado? Resposta: ["Postman Pat", "Spot"]

borrowers :: Database -> Book ->[Person] --função que recebe o banco de dados, o livro e retorna a pessoa que retirou o livro
borrowers [] _ = []
borrowers ((pessoa,liv):cauda) livro
        |(livro == liv) = pessoa : (borrowers cauda livro)
        |otherwise = borrowers cauda livro
-- Estudo de caso: tendo como entradas o nome do BD (exempleBase) e o nome de um livro ("All Alone") qual será o nome retornado? Resposta: ["Anna"]

borrowed :: Database -> Book -> Bool -- função que recebe o banco de dados, o nome do livro e verifica se o mesmo foi emprestado
borrowed [] _ = False
borrowed ((pessoa,liv):cauda) livro
        |(livro == liv) = True
        |otherwise = False
-- Estudo de caso: tendo como entradas o nome do BD (exempleBase) e o nome de um livro ("Dom Casmurro") qual será o status retornado? Resposta: False

numBorrowed :: Database -> Person -> Int -- função que recebe o banco de dados, o nome da pessoa e retorna a quantidade de livros emprestados
numBorrowed [] _ = 0
numBorrowed ((pes,liv):cauda) pessoa
        |(pessoa == pes) = length(books cauda pessoa)
        |otherwise = length(books cauda pessoa)
-- Estudo de caso: tendo como entradas o nome do BD (exempleBase) e o nome de uma pessoa ("Anna") qual será a quantidade de livros retirados? 
--Resposta: 1

makeLoan :: Database -> Person -> Book -> Database
makeLoan bd pessoa livro = (pessoa,livro) : bd -- função que recebe o BD, o nome da pessoa e o nome do livro e acrescenta ao BD.
-- Estudo de caso: tendo como entradas o nome do BD (exempleBase), o nome de uma pessoa ("Sara") e o nome do livro ("Dom Casmurro") acrescentar ao
--banco de dados. Resposta: [("Sara", "Dom Casmurro"), ("Alice", "Postman Pat"), ("Anna", "All Alone"), ("Alice","Spot"), ("Rory", "Postman Pat")]

returnLoan :: Database -> Person -> Book -> Database
returnLoan ((pes,liv):cauda) pessoa livro
            |(pessoa == pes) && (livro == liv) = returnLoan cauda (pessoa,livro)
            |otherwise = (pes,liv) : (returnLoan cauda (pessoa,livro))
