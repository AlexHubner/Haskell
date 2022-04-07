module Typechecer where

import AbsLI
import Prelude hiding (lookup)
import PrintLI

{-
(types, functions)
typechecker: lookup deep, update shallow,  lookup shallow, update shallow


(values,functions)
interpreter: lookup deep, update deep, lookup shallow, update shallow

-}

data R a = OK a | Erro String                                   
         deriving (Eq, Ord, Show, Read)



typeCheckP :: Program  -> [R Environment]
typeCheckP (Prog fs) = let environment = updatecF ([],[]) fs in
                          case environment of
                             OK env -> map (typeCheckF env) fs
                             Erro msg -> [Erro msg]
               
typeCheckC:: Environment -> ClassDeclaration -> R Environment
typeCheckC env (ClassD cid fs) = let environment = updatecF env fs in
                                  case environment of
                                     OK env -> OK env
                                     Erro msg -> Erro msg
                                     
typeCheckM :: Environment -> MethodDeclaration -> R Environment
typeCheckM env (Mth mid tp params stmts) = let environment = updatecM env mid tp params stmts in
                                               case environment of
                                                  OK env -> OK env
                                                  Erro msg -> Erro msg

typeCheckA :: Environment -> MemberDeclaration -> R Environment
typeCheckA env (Attr mid tp) = let environment = updatecA env mid tp in
                                 case environment of
                                    OK env -> OK env
                                    Erro msg -> Erro msg
                                    
tk :: Environment -> Stm -> R Environment
tk environment x = case x of
   cAss@(SAss id exp) -> let r = tinf environment exp in
                            case r of
                               OK x -> let r2 = tke environment (EVar id) x in 
                                          case r2 of 
                                             OK env -> OK env
                                             Erro msg -> Erro (msg ++ " no comando: " ++ printTree cAss)
                               Erro msg -> Erro (msg ++ " no comando: " ++ printTree cAss)
   SBlock [] -> OK environment
   SBlock ( cDec@(SDec (Dec tp id)):stms) -> let r = updateShallowType environment id tp in 
                                                  case r of 
                                                    OK env  -> tk env (SBlock stms)
                                                    Erro msg -> Erro (msg ++ " no comando: " ++ printTree cDec)
   SBlock (sb@(SBlock bls):stms) -> let r = tk (push environment) sb in
                                        case r of
                                           OK _ -> tk environment (SBlock stms)
                                           Erro msg -> Erro (msg ++ " no comando: " ++ printTree sb) 
   SBlock (s:stms) -> let r = tk environment s in
                         case r of 
                            OK _ -> tk environment (SBlock stms)
                            Erro msg -> Erro msg
   cWhile@(SWhile exp stm) ->  let r = tke environment exp Tint in 
                                  case r of 
                                     OK _ -> tk environment stm
                                     Erro msg -> Erro (msg ++ " no comando: " ++ printTree cWhile)
   cIf@(SIf exp stmT stmE) ->  let r = tke environment exp Tint in
                                  case r of
                                     OK _ -> let r2 = tk environment stmT in 
                                                case r2 of
                                                   OK _ ->  tk environment stmE
                                                   Erro msg -> Erro (msg ++ " no comando: " ++ printTree cIf)
                                     Erro msg -> Erro (msg ++ " no comando: " ++ printTree cIf)


tke :: Environment -> Exp -> Type -> R Environment
tke environment exp tp = let r = tinf environment exp in
                          case r of
                             OK tipo -> if tipo == tp 
                                           then OK environment
                                           else Erro (" A expressao "++ printTree exp ++ " tem tipo " ++ printTree tipo ++
                                                      " mas o tipo esperado eh " ++ printTree tp)
                             Erro msg -> Erro msg  

combChecks :: Environment -> Exp -> Exp -> Type -> R Type
combChecks environment exp1 exp2 tp = let r = tke environment exp1 tp in
                                       case r of
                                          OK _ -> let r2 = tke environment exp2 tp in
                                                     case r2 of 
                                                         OK _ -> OK tp
                                                         Erro msg -> Erro msg
                                          Erro msg -> Erro msg 


tinf :: Environment -> Exp -> R Type
tinf environment x  =  case x of
    ECon exp0 exp  -> combChecks environment exp0 exp TStr
    EAdd exp0 exp  -> combChecks environment exp0 exp Tint
    ESub exp0 exp  -> combChecks environment exp0 exp Tint
    EMul exp0 exp  -> combChecks environment exp0 exp Tint
    EDiv exp0 exp  -> combChecks environment exp0 exp Tint
    EOr  exp0 exp  -> combChecks environment exp0 exp Tbool
    EAnd exp0 exp  -> combChecks environment exp0 exp Tbool
    ENot exp       -> let r = tke environment exp Tbool in 
                         case r of 
                             OK _ -> OK Tbool
                             Erro msg -> Erro msg
    EStr str       -> OK TStr  
    ETrue          -> OK Tbool 
    EFalse         -> OK Tbool  
    EInt n         -> OK Tint  
    EVar id        -> lookupDeepType environment id
    EMthCall id0 id exp -> let r = lookupDeepType environment id in
                           case r of
                             OK tp -> let r2 = tinf environment exp in
                                       case r2 of
                                          OK _ -> OK tp
                                          Erro msg -> Erro msg
                             Erro msg -> Erro msg
    ENew id        -> let r = lookupDeepType environment id in
                          case r of
                             OK tp -> OK tp
                             Erro msg -> Erro msg

instance Show Ident where
  show (Ident s) = printTree s

instance Show Type where
  show (Tbool) = printTree Tbool
  show (Tint)  = printTree Tint
  show (Tvoid) = printTree Tvoid
  show (TStr)  = printTree TStr
  --show (TFun (Fun tp _ decls _)) = printTree tp ++ "<-" ++ "(" ++ printTree decls ++ ")" 

type Environment = ([RContext],RContext)
type RContext = [(Ident,Type)]

pushB :: RContext -> Environment -> Environment
pushB typeBindings (sc,fnCtx) = (typeBindings:sc,fnCtx) 

push :: Environment -> Environment
push (sc,fnCtx) = ([]:sc,fnCtx)
 
pop :: Environment -> Environment
pop ((s:scs),fnCtx) = (scs,fnCtx)

lookupDeepType :: Environment -> Ident -> R Type
lookupDeepType ([],fnCtx) id = Erro (printTree id ++ " nao esta no contexto. ")
lookupDeepType ((s:scs),fnCtx) id =  let r = lookupShallow s id in
                                         case r of
                                            OK tp -> OK tp
                                            Erro _ -> lookupDeepType (scs,fnCtx) id

lookupShallowFunction :: Environment -> Ident -> R Type
lookupShallowFunction (sc,fnCtx) id = lookupShallow fnCtx id

lookupShallow :: RContext -> Ident -> R Type
lookupShallow [] s = Erro (printTree s ++ " nao esta no contexto. ")
lookupShallow ((i,v):cs) s
   | i == s = OK v
   | otherwise = lookupShallow cs s

updateShallowType :: Environment -> Ident -> Type -> R Environment
updateShallowType ([],fnCtx) id tp = OK ([[(id,tp)]],fnCtx)
updateShallowType ((s:sc),fnCtx) id tp = let r = updateShallow s id tp  in 
                                             case r of 
                                                OK cxt -> OK (cxt:sc,fnCtx)
                                                Erro msg -> Erro msg

updateShallow :: RContext -> Ident -> Type -> R RContext
updateShallow context id tp = if (elem id (map fst context))
                                then Erro "tipo ja definido no contexto de tipos"
                                else OK ((id,tp):context)

updatecF :: Environment -> [Function] -> R Environment
updatecF e [] = OK e
updatecF (sc,c) (f@(Fun tp id params stms):fs) = let r = updateShallow c id (TFun f) in
                                                   case r of 
                                                     OK ctx -> updatecF (sc,ctx) fs
                                                     Erro msg -> Erro msg

