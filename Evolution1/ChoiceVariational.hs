module ChoiceVariational where

import Data.Maybe
import ChoiceCalculus

type Decision = [(Dim,Tag)] 
           
instance Eq (V a) where
 _ == _  = True 
 

        
semantics :: (V a) -> [(Decision, (V a))]
semantics (Obj v) = [([],Obj v)]
semantics (Dim dim tags v) = [( (dim,(elemAt i tags)) : d, p) | 
                              i <- [1..length tags], (d,p) <- semantics (choiceElimination dim i v)]

{- exercise the variability provided by the dimension, by eliminating each tag and
obtaining the semantics of the resulting expression.
-}
choiceElimination :: Dim -> Int -> (V a) -> (V a)
choiceElimination _ _ (Obj v) = Obj v 
choiceElimination dim i (Dim dim' tags v) 
                    | dim == dim' = Dim dim' tags v
                    | otherwise = Dim dim' tags (choiceElimination dim i v) 
choiceElimination dim i (Chc dim' vs) 
                    | dim == dim' = choiceElimination dim i (elemAt i vs)
                    | otherwise = Chc dim' (map (choiceElimination dim i) vs) 
                    
tagSelection :: Tag -> Dim -> (V a) -> (V a)
tagSelection tag dim v = choiceElimination dim i v'
      where    (tags,v')  = fromJust (find dim v)
               i = position tag tags 

find :: Dim -> (V a) -> Maybe ([Tag],(V a))  
find _    (Obj v) = Nothing
find dim (Dim dim' tags v) 
           | dim == dim' =  Just (tags,v) 
           | otherwise = find dim v 
find dim (Chc dim' vs) = head (dropWhile (== Nothing) 
                                         (map (find dim) vs))                                     

{-
By repeatedly selecting tags from dimensions, we will eventually produce a plain expression
-}
derivation :: Decision -> (V a) -> (V a)
derivation decision v = foldl (\ pv dimTag -> tagSelection (snd dimTag) (fst dimTag) pv) v decision         

---------------- examples -----------

ab = Dim "A" ["a1","a2"] 
         (Chc "A" [Dim "B" ["b1","b2"] (Chc "B" [Obj 1,Obj 2]) , 
                   Obj 3])

--ab = dimA $ chcA [dimB $ chcB [Obj 1, Obj 2], Obj 3]

                    
-------------- auxiliary functions ---------------                  
elemAt n (a:as)
   | n == 1  =  a
   | n  > 1  = elemAt (n-1) as 
   
position elem (a:as)
    | elem == a = 1
    | otherwise = 1 + position elem as 
   
{-
  derivation decision v ==  head [ pe | (d,pe) <- (semantics v), d == decision ]
-}   



{-  variational expressions

type VList a = V (List a)
data List a = Cons a (List a) |
              Empty           |
              VList (VList a)

len :: List a -> V Int
len Empty = int 0
len (Cons _ xs) = fmap (+1) (len xs)
len (VList vl) = vl >>= len

liftV :: (a -> V b) -> V a -> V b
liftV = flip (>>=)

vlen :: VList a -> V Int
vlen = liftV len
-}


{-
data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr deriving (Eq, Show)
			
eval :: Expr -> Int
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
-}

type VExpr = V Expr

data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr |
			VExpr VExpr deriving (Eq, Show)
			
							
eval :: Expr -> V Int
eval (Lit i) = Obj i
eval (Add e1 e2) = eval e1 <+> eval e2 -- (fmap (+) eval e1) <*> eval e2
eval (Sub e1 e2) = eval e1 <-> eval e2
eval (VExpr ve)  = ve >>= eval 

 --(>>=)  :: Monad m => m a -> (a -> m b) -> m b
 --(<*>) :: Applicative f => f (a -> b) -> f a -> f b

veval :: VExpr  -> V Int 
veval = liftV eval 

(<+>) :: V Int -> V Int -> V Int
l1 <+> l2 =  (fmap (+) l1) <*> l2
-- l1 = [1,2] ; l2 = [3,4,5]  
-- l12 = [4,5,6,5,6,7]
-- f l1 l2 = [ a+b | a <- l1, b <- l2 ]
 

--fmap (ee2 -> fmap (\ee1 -> (ee1+ee2) e1)) 
--     e2            