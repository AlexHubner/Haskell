import ChoiceCalculus
import ChoiceVariational

import Data.Map hiding (filter)

--DATA DEFINITIONS
data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr |
            Mul Expr Expr |
            Div Expr Expr |
            VExpr VExpr deriving (Show)

--TYPE DEFINITIONS

type Cache = [(Expr,Int)] 
type PreOrderLocation = Int
type ParticularEvolution = Expr -> PreOrderLocation -> Expr -> Expr
type VExpr = V Expr

--FUNCTIONS

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2 
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

evolution :: ParticularEvolution 
evolution expI loc nexp = fst (evolAux expI loc nexp)

evolAux :: Expr -> PreOrderLocation -> Expr -> (Expr,Int)
evolAux expI loc nexp
  | loc == 0 = (nexp,0)
  | loc > 0 = case expI of
                 Lit _          -> (expI, loc-1)
                 Add left right -> let (leftWalk, nLoc) = evolAux left (loc-1) nexp in
                                     if (nLoc == 0)
                                       then (Add leftWalk right, 0)
                                       else let (rightWalk, rnLoc) = evolAux right nLoc nexp in
                                                                      (Add left rightWalk, rnLoc)

evalIdeal:: Expr -> Cache -> Expr -> Int  
evalIdeal expI (cache) nexp
    | expI == nexp =  (filter (\x -> x == nexp) cache)
    | otherwise = let (nexpWalk, nLoc) = evolAux expI (length cache) nexp in
                     evalIdeal nexpWalk (cache++[(nexpWalk,eval nexpWalk)]) nexp
{-- filter retorna uma lista construída a partir de membros de uma lista (o segundo argumento) cumprindo
uma condição dada pelo primeiro argumento
fst retorna o primeiro item em uma tupla--}
   
evalEA :: Expr -> Cache -> ParticularEvolution -> PreOrderLocation -> Expr -> Int  
evalEA expI cache evolution loc nexp = 
    let evolvedModel = evolution expI loc nexp 
        analysisLocalChange = eval nexp  
    in
    eval'' evolvedModel (updateCache cache nexp analysisLocalChange)

updateCache :: Cache -> Expr -> Int -> Cache
updateCache cache nexp analysisLocalChange = 
    let (nexpWalk, nLoc) = evolAux nexp (length cache) nexp in
    cache ++ [(nexpWalk, analysisLocalChange)]

eval'' :: Expr -> Cache -> Int
eval'' (Add e1 e2) cache = re1 + re2 
-- other cases similar
  where 
       re1 = if (e1 `elem` cache) then (cache e1) else (eval'' cache e1)
       re2 = if (e2 `elem` cache) then (cache e2) else (eval'' cache e2)

evol :: Expr -> Int -> Expr -> Expr
evol expI loc nexp = fst (evolAux expI loc nexp)



--VARIATIONAL FUNCTIONS
veval :: VExpr -> Int
veval = liftV eval

evalV :: VExpr -> V Int
evalV (Lit i) = Obj i
evalV (Add e1 e2) = (+) <$> evalV e1 <*> evalV e2
evalV (Sub e1 e2) = (-) <$> evalV e1 <*> evalV e2
evalV (VExpr ve) = evalV ve