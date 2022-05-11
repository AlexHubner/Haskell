module Evolucao4 where

import ChoiceCalculus

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
(<->) :: V Int -> V Int -> V Int
l1 <-> l2 = (fmap (-)l1) <*> l2

(<+>) :: V Int -> V Int -> V Int
l1 <+> l2 =  (fmap (+) l1) <*> l2

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2 
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

eval2 :: Expr -> V Int
eval2 (Lit i) = Obj i
eval2 (Add e1 e2) = eval2 e1 <+> eval2 e2 -- (fmap (+) eval e1) <*> eval e2
eval2 (Sub e1 e2) = eval2 e1 <-> eval2 e2
eval2 (VExpr ve)  = ve >>= eval2 

evolution :: ParticularEvolution 
evolution expI loc nexp =  case expI of
                              Lit i -> Lit i
                              Add e1 e2 -> Add (evolution e1 loc nexp) (evolution e2 loc nexp)
                              Sub e1 e2 -> Sub (evolution e1 loc nexp) (evolution e2 loc nexp)
                              Mul e1 e2 -> Mul (evolution e1 loc nexp) (evolution e2 loc nexp)
                              Div e1 e2 -> Div (evolution e1 loc nexp) (evolution e2 loc nexp)  


evolAux :: Expr -> PreOrderLocation -> Expr -> (Expr,Int) --tornar VExpr
evolAux expI loc nexp
  | loc == 0 = (nexp,0)
  | loc > 0 = case expI of
                 Lit _          -> (expI, loc-1)
                 Add left right -> let (leftWalk, nLoc) = evolAux left (loc-1) nexp in
                                     if (nLoc == 0)
                                       then (Add leftWalk right, 0)
                                       else let (rightWalk, rnLoc) = evolAux right nLoc nexp in
                                                                      (Add left rightWalk, rnLoc)

evalIdeal:: Expr -> Cache -> Expr -> Int  --tornar VExpr
evalIdeal expI (cache) nexp = let (nexp',nLoc) = evolAux expI (length cache) nexp in
                                 if (nLoc == 0)
                                   then eval nexp'
                                   else let (cache',nLoc') = evolAux expI nLoc nexp in
                                            evalIdeal expI cache nexp'

evalEA :: Expr -> Cache -> ParticularEvolution -> PreOrderLocation -> Expr -> Int  --tornar VExpr
evalEA expI cache evolution loc nexp = 
    let evolvedModel = evolution expI loc nexp 
        analysisLocalChange = eval nexp  
    in
    eval'' evolvedModel (updateCache cache nexp analysisLocalChange)

teste2 = evalEA (Add (Lit 1) (Lit 2)) [] evolution 0 (Lit 3)

updateCache :: Cache -> Expr -> Int -> Cache --tornar VExpr
updateCache cache nexp analysisLocalChange = 
    let (nexpWalk, nLoc) = evolAux nexp (length cache) nexp in
    cache ++ [(nexpWalk, analysisLocalChange)]

eval'' :: Expr -> Cache -> Int
eval'' (Add e1 e2) cache = re1 + re2
  where
    re1 = evalEA e1 cache evolution 0 e2
    re2 = evalEA e2 cache evolution 0 e1
eval'' (Sub e1 e2) cache = re1 - re2
  where
    re1 = evalEA e1 cache evolution 0 e2
    re2 = evalEA e2 cache evolution 0 e1
eval'' (Mul e1 e2) cache = re1 * re2
  where
    re1 = evalEA e1 cache evolution 0 e2
    re2 = evalEA e2 cache evolution 0 e1
eval'' (Div e1 e2) cache = re1 `div` re2
  where
    re1 = evalEA e1 cache evolution 0 e2
    re2 = evalEA e2 cache evolution 0 e1
eval'' (Lit i) cache = i

evol :: Expr -> Int -> Expr -> Expr --tornar VExpr
evol expI loc nexp = do 
    let (nexp',nLoc) = evolAux expI loc nexp
    if (nLoc == 0)
      then nexp'
      else let (cache',nLoc') = evolAux expI nLoc nexp in
            evol expI nLoc' cache'