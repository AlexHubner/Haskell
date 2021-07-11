-- Somar elementos de uma lista:
sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

-- Dobrar os elementos de uma lista:
dobrar :: [Int] -> [Int]
dobrar [] = []
dobrar (x:xs) = 2 * x : dobrar xs

-- Perterncer: checar se um elemento está na lista:
pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence a (x:xs)
           | a == x  =  True
           | otherwise = pertence a xs

-- Filtragem: apenas os números de uma string
--import Data.Char

--digitos :: String -> String
--digitos [] = []
--digitos (a:as)
--            | isDigit a = a : digitos as
--            | otherwise = digitos as


-- Soma de uma lista de pares
somarPares :: [(Int,Int)] -> [Int]
somarPares [] = []
somarPares (a,b):xs = (a + b) + somarPares xs

-- Comprimento
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (a:as) = 1 + tamanho as

-- COMPREENSÕES DE LISTAS

dobrarListas x = [2*a|a <- x]

somarPares2 :: [(Int, Int)] -> [Int]
somarPares2 x = [a+b | (a,b) <- x]