{-# LANGUAGE BlockArguments #-}
module Typechecer3 where

import AbsLI
import Prelude hiding (lookup)
import PrintLI
import Prelude (lookup)

data R a = OK a | Erro String
         deriving (Eq, Ord, Show, Read)

type Environment = ([RContext],RContext)
type RContext = [(Ident,Type)]

--TYPECHECKERS
typeCheckP :: Program -> R Environment
typeCheckP (Prog classe) = let env = updateClass ([],[]) classe in
    case env of
        (Erro s) -> Erro s
        (OK env) -> typeCheckClass env classe


typeCheckClass :: Environment -> [ClassDeclaration] -> R Environment
typeCheckClass env [] = OK env
typeCheckClass env (c:cs) = let env' = typeCheckClass env cs
                              in case env' of
                                     (OK env) -> let env'' = typeCheckClass env cs
                                                 in case env'' of
                                                      (OK env'') -> OK env''
                                                      (Erro msg) -> Erro msg

typeCheckM :: Environment -> MemberDeclaration -> R Environment
typeCheckM environment (Attr (Dec tp id)) = if (tp /= Tvoid)
                                            then OK (pushB [(id,tp)] environment)
                                            else Erro ("Atributo " ++ show id ++ " nao pode ser do tipo void")
typeCheckM environment (Mth tp id decl stms) = if (stms /= [])
                                                then let lastCommand = last stms in
                                                         case lastCommand of
                                                            SReturn exp -> let r = tk newEnvironment (SBlock stmsButLast) in
                                                                              case r of
                                                                                 OK env -> tke env exp tp
                                                                                 Erro msg -> Erro msg
                                                                           where stmsButLast  = reverse (tail (reverse stms))
                                                            _           -> Erro ("Na funcao " ++ show id ++ ", nao ha comando retornando valor do tipo " ++ show tp)
                                                else Erro ("Nao ha comando retornando valor do tipo da funcao " ++ show tp)
                                             where newEnvironment = pushB [(id,tp)] environment

--FUNCTIONS

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
                             OK tipo -> if (tipo == tp )
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
    EMthCall id0 id lexp   -> let r = lookupShallowFunction environment id in
                                 case r of
                                    OK (TFun (Fun tipoRetorno _ decls stms)) -> if (length decls == length lexp)
                                                                                 then
                                                                                    if ( isThereError tksArgs /= [])
                                                                                       then Erro "chamada de funcao invalida"
                                                                                       else OK tipoRetorno
                                                                                 else Erro " tamanhos diferentes de lista de argumentos e parametros"
                                                                                 where
                                                                                       parameterTypes =  map (\(Dec tp _ )-> tp) decls
                                                                                       tksArgs = zipWith (tke environment) lexp parameterTypes
                                                                                       isThereError l = filter (==False) (map (\e->(let r2 = e in
                                                                                                                                    case r2 of
                                                                                                                                       OK _ -> True
                                                                                                                                       Erro _ -> False))
                                                                                                                              l)

                                    Erro msg -> Erro msg

--INSTANCES

instance Show Ident where
  show (Ident s) = printTree s

instance Show Type where
  show (Tbool) = printTree Tbool
  show (Tint)  = printTree Tint
  show (Tvoid) = printTree Tvoid
  show (TStr)  = printTree TStr
  show (TFun (Fun tp _ decls _)) = printTree tp ++ "<-" ++ "(" ++ printTree decls ++ ")"
  show (TClass id) = printTree id



--PUSH FUNCTIONS

pushB :: RContext -> Environment -> Environment
pushB typeBindings (sc,fnCtx) = (typeBindings:sc,fnCtx)

push :: Environment -> Environment
push (sc,fnCtx) = ([]:sc,fnCtx)

pop :: Environment -> Environment
pop ((s:scs),fnCtx) = (scs,fnCtx)

--LOOK AND UPDATE FUNCTIONS

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

updateClass :: Environment -> [ClassDeclaration] -> R Environment
updateClass e [] = OK e
updateClass (sc,c) (f@(ClassD id corpo):fs) = let r = updateShallow c id (TClass id) in
                                                case r of
                                                  OK ctx -> updateClass (sc,ctx) fs
                                                  Erro msg -> Erro msg