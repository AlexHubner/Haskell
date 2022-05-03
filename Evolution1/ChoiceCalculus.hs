module ChoiceCalculus where

--TYPE DEFINITIONS

type Dim = String
type Tag = String
type VList a = V (List a) --Listas variacionais

--DATA DEFINITIONS
data V a = Obj a
        |Dim Dim [Tag] (V a)
        |Chc Dim [V a] deriving (Show)

data List a = Cons a (List a)
            | Empty
            | VList (VList a) deriving (Show)

-- INSTANCES
instance Functor V where
    fmap f (Obj x)       = Obj $ f x
    fmap f (Dim d ts vs) = Dim d ts $ fmap f vs
    fmap f (Chc d vs)    = Chc d $ fmap (fmap f) vs

bindV :: V a -> (a -> V b) -> V b
Obj a     `bindV` f = f a
Dim d t v `bindV` f = Dim d t (v >>= f)
Chc d vs  `bindV` f = Chc d (map (>>= f) vs)

instance Applicative V where
  pure = Obj
  mf <*> mx = mf `bindV` (\f -> mx `bindV` (\x -> pure $ f x))

instance Monad V where
  (>>=) = bindV

--AUXILIRIES FUNCTIONS

atomic :: Dim -> [Tag] -> [V a] -> V a
atomic d ts cs = Dim d ts $ Chc d cs

--Funções simples de Haskell que depois serão "levantadas" (lift) para o tipo V
list :: List a -> VList a
list = Obj

single :: a -> List a
single a = Cons a Empty

many :: [a] -> List a
many = foldr Cons Empty

--Funções auxiliares para listas variacionais
vempty :: VList a       -- representa uma lista variacional vazia
vempty = list Empty

vsingle :: a -> VList a -- constroi uma lista variacional contendo um elemento
vsingle = list . single

vcons :: a -> VList a -> VList a -- pega um elemento e o adiciona ao início da lista variacional
vcons x = list . Cons x . VList

vlist :: [a] -> VList a -- transforma uma lista Haskell em uma VList
vlist = list . many

--Definir uma função para apresentar um recurso opcional
opt :: Dim -> a -> VList a
opt d x = atomic d ["yes","no"] [vsingle x, vempty]

{--Definição de listas variacionais marcadas para dar uma definição mais modular ao destinarmos duas
opções de menu separadas e as combinarmos--}

type Tagged a = (Tag, V a)

infixl 2 <:

(<:) :: Tag -> V a -> Tagged a
t <: v = (t,v)

--Função para combinar uma lista de alternativas marcadas em uma dimensão
alt :: Dim -> [Tagged a] -> V a
alt d tvs = atomic d ts vs where (ts,vs) = unzip tvs

--Função len para lista variacional usando bind monatico
len :: List a -> V Int
len Empty = Obj 0
len (Cons _ xs) = fmap (+1) (len xs)
len (VList vl) = vl >>= len

--Função geral para aplicar len à listas variacionais
liftV :: (a -> V b) -> V a -> V b
liftV = flip (>>=)

--Função que serve como versão adicional necessária a função len
vlen :: VList a -> V Int
vlen = liftV len

--Função para concatenação de listas
cat :: List a -> List a -> List a
cat Empty r = r
cat (Cons a l) r = Cons a (l `cat` r)
cat (VList vl) r = VList (fmap (`cat` r) vl)

--Função com a mesma ação de cat só que para listas variacionais
vcat :: VList a -> VList a -> VList a
vcat l r = list $ cat (VList l) (VList r)

--Funções para encontrar o enésimo elemento de uma lista variacional
nth :: Int -> List a -> V a
nth _ Empty = undefined
nth 1 (Cons x _) = Obj x
nth n (Cons _ xs) = nth (n-1) xs
nth n (VList vl) = vl >>= nth n

vnth :: Int -> VList a -> V a
vnth n = liftV (nth n)

--Função fold para listas variacionais
fold :: (a -> b -> b) -> b -> List a -> V b
fold _ b Empty = Obj b
fold f b (Cons a l) = fmap (f a) (fold f b l)
fold f b (VList vl) = vl >>= fold f b