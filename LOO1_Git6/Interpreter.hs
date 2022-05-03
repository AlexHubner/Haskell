module Interpreter where

import Data.List
import AbsLI
import Prelude hiding (lookup)

prog = Prog [ClassD (Ident "Test") [Mth Tvoid (Ident "main") [] [SDec (Dec (TClass (Ident "Conta")) (Ident "conta1")),SDec (Dec (TClass (Ident "Conta")) (Ident "conta2")),SAss (Ident "conta1") (ENew (Ident "Conta")),SExp (EMthCall (Ident "conta1") (Ident "creditar") [EInt 100]),SAss (Ident "conta2") (ENew (Ident "Conta")),SExp (EMthCall (Ident "conta2") (Ident "creditar") [EInt 200]),SExp (EMthCall (Ident "conta1") (Ident "transferir") [EVar (Ident "conta2"),EInt 50])]],ClassD (Ident "Conta") [Attr (Dec Tint (Ident "saldo")),Mth Tvoid (Ident "creditar") [Dec Tint (Ident "valor")] [SAss (Ident "saldo") (EAdd (EVar (Ident "saldo")) (EVar (Ident "valor")))],Mth Tvoid (Ident "debitar") [Dec Tint (Ident "valor")] [SAss (Ident "saldo") (ESub (EVar (Ident "saldo")) (EVar (Ident "valor")))],Mth Tvoid (Ident "transferir") [Dec (TClass (Ident "Conta")) (Ident "destino"),Dec Tint (Ident "valor")] [SExp (EMthCall (Ident "this") (Ident "debitar") [EVar (Ident "valor")]),SExp (EMthCall (Ident "destino") (Ident "creditar") [EVar (Ident "valor")])]]]


executeP :: Program -> Environment

{- searches for and executes the main method in any class -}
executeP (Prog cds) =  execute (updatecF initialEnvironment cds) ( SBlock (stmMain cds))
    where stmMain ( (ClassD _ members) : cds) = let searchMainClass = filter (\ m -> case m of 
                                                                                       Mth _ (Ident "main") _ stms -> True
                                                                                       _ -> False 
                                                                             ) members in
                                                    case searchMainClass of 
                                                      [] -> stmMain cds 
                                                      _  -> (\(Mth _ (Ident "main") _ stms) -> stms) (head searchMainClass)

execute :: Environment -> Stm -> Environment
execute environment x = case x of
   SExp exp ->  snd (eval environment exp) 
   SDec (Dec tp id) -> updateShallowValue environment (Iden id) (initVal tp)
   SAss id exp -> let (valor,nenv) = eval environment exp  in  
                     updateDeepValue nenv (Iden id) valor
   SBlock [] -> environment
   SBlock (sb@(SBlock bls):stms) -> execute (pop (execute (push environment) sb)) (SBlock stms)
   SBlock (s:stms) -> execute (execute environment s) (SBlock stms) 
   SWhile exp stm -> let (valor,nenv) = eval environment exp in 
                        if ( i (valor) /= 0) 
                         then execute (execute nenv stm) (SWhile exp stm)
                         else nenv
   SReturn exp ->  let (valor,nenv) = eval environment exp in 
                      updateShallowValue nenv  (Iden (Ident "return"))  valor
   SIf exp stmT stmE -> let (valor,nenv) = eval environment exp in 
                         if ( i (valor) /= 0) 
                           then execute nenv stmT
                           else execute nenv stmE

eval :: Environment -> Exp -> (Valor,Environment)
eval env x = case x of
    ECon exp0 exp  -> (ValorStr ( (s valor1) ++  (s valor2)), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                           (valor2,nenv2) = (eval nenv1 exp)
    EAdd exp0 exp  -> (ValorInt ( (i valor1) + (i valor2) ), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                          (valor2,nenv2) = (eval nenv1 exp)
    ESub exp0 exp  -> (ValorInt ( (i valor1) - (i valor2) ), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                          (valor2,nenv2) = (eval nenv1 exp)
    EMul exp0 exp  -> (ValorInt ( (i valor1) * (i valor2) ), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                          (valor2,nenv2) = (eval nenv1 exp)
    EDiv exp0 exp  -> (ValorInt ( (i valor1) `div` (i valor2) ), nenv2)  where (valor1,nenv1) = (eval env exp0)
                                                                               (valor2,nenv2) = (eval nenv1 exp)
    EOr  exp0 exp  -> (ValorBool ( (b valor1) || (b valor2) ), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                            (valor2,nenv2) = (eval nenv1 exp)
    EAnd exp0 exp  -> (ValorBool ( (b valor1) && (b valor2) ), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                            (valor2,nenv2) = (eval nenv1 exp)
    ENot exp0       -> (ValorBool (not (b valor1)), nenv1) where (valor1,nenv1) = (eval env exp0)
    EStr str       -> (ValorStr str, env)
    ETrue          -> (ValorBool True, env)
    EFalse         -> (ValorBool False, env)
    EInt n         -> (ValorInt n, env )
    EVar id        -> (lookupDeepValue env  (Iden id), env)
    ENew classN    -> (ValorLvalue lvalue, newEnvironment)
                      where
                       lvalue = getNewLvalue (trd env) 
                       newEnvironment = (fs env, sn env, newHeap)
                       newHeap = updateShallow (trd env) lvalue (ObjectState objState) 
                       objState = typeAnnotation : intialAttributes
                       intialAttributes = map (\ (Attr (Dec tp id)) -> (Iden id, initVal tp) )   
                                               (getAttributes (cl( (\(OK val) -> val)(lookupShallow (sn env) (Iden classN))))) 
                       typeAnnotation = (Iden (Ident "_type"), (\(Ident str) -> ValorStr str) classN)                                  
    EMthCall obj mth lexp -> (lookupShallowValue finalEnv (Iden (Ident "return")) , newEnvironment)
                          where lvalue = lvl (lookupDeepValue env (Iden obj))
                                objState = objRc ( (\(OK val) -> val) (lookupShallow (trd env) lvalue) )
                                classN = Iden (Ident (s((\(OK val) -> val)(lookupShallow objState (Iden (Ident "_type"))))))
                                (decls, stms) = getMethodInfo mth (cl( (\(OK val) -> val)(lookupShallow (sn env) classN)))
                                callEnv = pushB (this:paramBindings) (pushB objState ([],sn env,trd env) )
                                this = (Iden (Ident "this"), (ValorLvalue lvalue))
                                paramBindings = zip (map (\(Dec _ id) -> Iden id) decls) 
                                                    (map (\ arg -> fst (eval env arg)) lexp)
                                finalEnv = execute callEnv (SBlock stms)
                                finalObjState = objRc ( (\(OK val) -> val) (lookupShallow (trd finalEnv) lvalue) )
                                deltaObjState = filter (\(at,v) -> v /= ((\(OK val) -> val) (lookupShallow objState at)) ) finalObjState
                                newObjState = top (pop finalEnv)
                                updatedNewObjState = foldl (\ objSt (at,nv) -> updateShallow objSt at nv ) 
                                                           newObjState deltaObjState
                                newEnvironment = (fs env ,sn env,updateShallow (trd finalEnv) lvalue (ObjectState updatedNewObjState))
                                


getMethodInfo :: Ident -> ClassDeclaration -> ([Decl] , [Stm])
getMethodInfo method (ClassD cn ((Mth _ mth decls stms):ms)) = if (method == mth) 
                                                                      then (decls,stms)
                                                                      else getMethodInfo method (ClassD cn ms)
getMethodInfo method (ClassD cn ((Attr _ :ms)) ) = getMethodInfo method (ClassD cn ms)

getAttributes :: ClassDeclaration -> [MemberDeclaration]
getAttributes (ClassD cn classMembers) = filter (\ cm -> case cm of 
                                                           Attr _ -> True
                                                           _ -> False)
                                                classMembers
                                            

getNewLvalue :: Heap -> Lvalue
getNewLvalue heap = if (heap /= []) 
                      then Objref ( ( (\ (Objref vint) -> vint) (fst (last (sort heap)))) + 1)
                      else Objref (1)
                                        

data Valor = ValorInt { i :: Integer } | 
             ValorStr { s :: String } | 
             ValorBool { b :: Bool} |
             ValorClass {cl :: ClassDeclaration} |            
             ObjectState {objRc :: RContext} |
             ValorLvalue {lvl :: Lvalue} 
                deriving Eq

initVal :: Type -> Valor
initVal Tbool = ValorBool False
initVal Tint  = ValorInt 0
initVal TStr  = ValorStr ""
initVal (TClass _) = ObjectState []

instance Show Ident where
  show (Ident s) =  s

instance Ord Valor where 
  _ <= _ = True

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = show s
  show (ValorLvalue lvl) = show lvl
  show (ValorClass cd ) = show cd
  show (ObjectState rc) = show rc
  
  
instance Show ClassDeclaration where
  show (ClassD ident members) =   "class " ++ show ident


data Lvalue =  Iden Ident 
             | Objref Integer 

instance Eq Lvalue where 
 (Iden lv1) == (Iden lv2) = lv1 == lv2
 (Objref lv1) == (Objref lv2) = lv1 == lv2
 _  ==  _  = False

instance Ord Lvalue where 
 (Iden lv1) <= (Iden lv2) = lv1 <= lv2
 (Objref lv1) <= (Objref lv2) = lv1 <= lv2

instance Show Lvalue where
  show (Iden ident) = show ident 
  show (Objref vint) = "#ref "++ show vint

-- an environment has a nested context, class declartions, and a heap.
type RContext = [(Lvalue,Valor)]
type Heap = RContext
type Environment = ([RContext],RContext,Heap)
initialEnvironment = ([],[],[])

 
--instance Eq ClassDeclaration where  
--  _ == _ = True
 
trd (_,_,heap) = heap
fs  (nestedContext,_,_) = nestedContext
sn  (_,classContext,_) = classContext

pushB :: RContext -> Environment -> Environment
pushB topBindings (sc,fnCtx,heap) = (topBindings:sc,fnCtx,heap) 

push :: Environment -> Environment
push (sc,fnCtx,heap) = ([]:sc,fnCtx,heap)
 
pop :: Environment -> Environment
pop ((s:scs),fnCtx,heap) = (scs,fnCtx,heap)

top :: Environment -> RContext
top ((s:_),_,_) = s 

data R a = OK a | Erro String                                   
         deriving (Eq, Ord, Show, Read)

--lookupDeepValue :: Environment -> Ident -> Valor
--lookupDeepType ([],fnCtx) id = Erro (printTree id ++ " nao esta no contexto. ")
lookupDeepValue ((s:scs),fnCtx,h) id =  let r = lookupShallow s id in
                                         case r of
                                            OK val -> val
                                            Erro _ -> lookupDeepValue (scs,fnCtx,h) id

--lookupShallowValue :: Environment -> Ident -> Valor   
lookupShallowValue  ((s:sc),_,_) id =  (\(OK val) -> val) (lookupShallow s id)
                                      
--lookupShallowFunction :: Environment -> Ident -> Valor
lookupShallowFunction (_,fnCtx) id = (\(OK val) -> val) (lookupShallow fnCtx id)

--lookupShallow :: RContext -> Ident -> R Valor
lookupShallow [] s = Erro (show s ++ " nao esta no contexto. ")
lookupShallow ((i,v):cs) s
   | i == s =  OK v
   | otherwise = lookupShallow cs s

--updateShallowValue :: Environment -> Ident -> Valor -> Environment
updateShallowValue ([],fnCtx,heap) id tp = ([[(id,tp)]],fnCtx,heap)
updateShallowValue ((s:sc),fnCtx,heap) id tp = ( (updateShallow s id tp):sc , fnCtx,heap)   

--updateDeepValue :: Environment -> Ident -> Valor -> Environment
updateDeepValue ([],fnCtx,heap) id tp = ([[(id,tp)]],fnCtx,heap)
updateDeepValue ((s:sc),fnCtx,heap) id tp = let r = lookupShallow s id in 
                                           case r of
                                               OK _ -> ( (updateShallow s id tp):sc , fnCtx, heap)
                                               Erro _ -> pushB s (updateDeepValue (sc,fnCtx,heap) id tp)    
                                             
--note that, differently from the typechecker, updating an existing value is possible
--updateShallow :: RContext -> Ident -> Valor -> RContext
updateShallow [] s nv = [(s,nv)]
updateShallow ((i,v):cs) s nv
        | i == s = (i,nv):cs
        | otherwise = (i,v) : updateShallow cs s nv
 
updatecF :: Environment -> [ClassDeclaration] -> Environment
updatecF e [] = e
updatecF (sc,c,h) (cd@(ClassD id members ):cds) = updatecF (sc, updateShallow c (Iden id) (ValorClass cd), h) cds
                                                     

