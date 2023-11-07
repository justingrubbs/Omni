-- {-# LANGUAGE RankNTypes #-}

module Omni.Python
   (
      pyParse, printPython, checkPyParse
   )
   where


import            Omni.Data
import qualified  Language.Python.Version3.Parser     as P
import qualified  Language.Python.Common              as PC


pyParse :: String -> String -> Either String Prog
pyParse txt f = 
   case P.parseModule txt f of 
      Left  err   -> error (show err)
      Right (p,q) -> Right (convertPythonAST p)

convertExpr :: PC.ExprSpan -> Expr
convertExpr (PC.Var (PC.Ident name _) _) = EVar name
convertExpr (PC.Int n _ _) = EInt n
convertExpr (PC.Bool b _) = EBool b
convertExpr _ = OtherE

-- Conversion function for statements
convertStmt :: PC.StatementSpan -> Stmt
convertStmt (PC.Assign targets expr _) = 
   case targets of
      [PC.Var (PC.Ident name _) _]  -> Assign name (convertExpr expr)
      _                             -> OtherS
convertStmt (PC.Print _ (expr : _) _ _) = Output (convertExpr expr) -- Might be wrong, idk what srcSpan is 
convertStmt _ = OtherS

convertStatements :: [PC.StatementSpan] -> [Stmt]
convertStatements = map convertStmt 
   -- so every time I have manually iterated through a list and 
   -- did some stuff I could've just used map?

convertPythonAST :: PC.ModuleSpan -> [Stmt]
convertPythonAST (PC.Module prog) = convertStatements prog

checkPyParse :: FilePath -> IO ()
checkPyParse file = do
   s <- readFile file
   case pyParse s file of
      Left err -> print err
      Right p  -> print p


-- Pretty-Printing:
---------------------------------------------------------------------

-- Will be used to add def main(): to beginning and main() and such.
printPython :: Prog -> String 
printPython = prettyPython ""

prettyPython :: String -> Prog -> String
prettyPython = foldl (\ text x -> text ++ printStmt x ++ "\n")

printStmt :: Stmt -> String
printStmt (Assign var e)     = var ++ " = " ++ printExpr e
printStmt (Declare ty var)   = var ++ ": " ++ printType ty
printStmt (DAndA ty var e)   = var ++ ": " ++ printType ty ++ " = " ++ printExpr e
printStmt (Output e)         = "print(" ++ printExpr e ++ ")"

printType :: Type -> String
printType TyInt  = "int"
printType TyBool = "bool"
printType Poly   = "<Type unknown>"
printType (TVar t) = "Type variable: " ++ show t 

printExpr :: Expr -> String
printExpr (EInt n)      = show n
printExpr (EBool True)  = "True"
printExpr (EBool False) = "False"
printExpr (EVar v)      = v
printExpr (Bin bop x y) = printExpr x ++ printBop bop ++ printExpr y
printExpr (Un uop x)    = printUop uop ++ printExpr x

printBop :: BOp -> String 
printBop Add = " + "
printBop Sub = " - "
printBop Mul = " * "
printBop Div = " / "
printBop Mod = " % "
printBop And = " and "
printBop Or  = " or "
printBop Eq  = " == "
printBop NEq = " != "
printBop Less = " < "
printBop Greater = " > "
printBop LessEq = " <= "
printBop GreaterEq = " >= "

printUop :: UOp -> String 
printUop Not = "not "
printUop Neg = "-"

