import Data.Maybe

type Dim = String
type Tag = String
type Decision = [(Dim,Tag)] 


data V a = Obj a |
           Dim Dim [Tag] (V a) |
           Chc Dim [V a] deriving (Show)
           
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

---------                ---------------
