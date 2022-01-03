-- Haskell module generated by the BNF converter

module SkelLI where

import qualified AbsLI

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsLI.Ident -> Result
transIdent x = case x of
  AbsLI.Ident string -> failure x
transProgram :: AbsLI.Program -> Result
transProgram x = case x of
  AbsLI.Prog classdeclarations -> failure x
transClassDeclaration :: AbsLI.ClassDeclaration -> Result
transClassDeclaration x = case x of
  AbsLI.ClassD ident memberdeclarations -> failure x
transMemberDeclaration :: AbsLI.MemberDeclaration -> Result
transMemberDeclaration x = case x of
  AbsLI.Attr decl -> failure x
  AbsLI.Mth type_ ident decls stms -> failure x
transDecl :: AbsLI.Decl -> Result
transDecl x = case x of
  AbsLI.Dec type_ ident -> failure x
transStm :: AbsLI.Stm -> Result
transStm x = case x of
  AbsLI.SDec decl -> failure x
  AbsLI.SExp exp -> failure x
  AbsLI.SAss ident exp -> failure x
  AbsLI.SBlock stms -> failure x
  AbsLI.SWhile exp stm -> failure x
  AbsLI.SReturn exp -> failure x
  AbsLI.SIf exp stm1 stm2 -> failure x
transExp :: AbsLI.Exp -> Result
transExp x = case x of
  AbsLI.EOr exp1 exp2 -> failure x
  AbsLI.EAnd exp1 exp2 -> failure x
  AbsLI.ENot exp -> failure x
  AbsLI.ECon exp1 exp2 -> failure x
  AbsLI.EAdd exp1 exp2 -> failure x
  AbsLI.ESub exp1 exp2 -> failure x
  AbsLI.EMul exp1 exp2 -> failure x
  AbsLI.EDiv exp1 exp2 -> failure x
  AbsLI.EMthCall ident1 ident2 exps -> failure x
  AbsLI.ENew ident -> failure x
  AbsLI.EInt integer -> failure x
  AbsLI.EVar ident -> failure x
  AbsLI.EStr string -> failure x
  AbsLI.ETrue -> failure x
  AbsLI.EFalse -> failure x
transType :: AbsLI.Type -> Result
transType x = case x of
  AbsLI.Tbool -> failure x
  AbsLI.Tint -> failure x
  AbsLI.Tvoid -> failure x
  AbsLI.TStr -> failure x
  AbsLI.TClass ident -> failure x

