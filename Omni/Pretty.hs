{-# LANGUAGE GADTs #-}

module Omni.Pretty where 


import Omni.Data 


data PrettyError where 
   BadStmt :: Stmt -> PrettyError 
   BadExpr :: Expr -> PrettyError 
   BadType :: Type -> PrettyError 
   BadLit  :: Literal -> PrettyError 
   BadBop  :: BOp -> PrettyError 
   BadUop  :: UOp -> PrettyError 
   BadAop  :: AOp -> PrettyError 
   Misc    :: String -> PrettyError
   deriving Show

