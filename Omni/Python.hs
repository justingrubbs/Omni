-- {-# LANGUAGE RankNTypes #-}

module Omni.Python
   (
      pyParse, printPython, checkPyParse
   )
   where


import            Omni.Data
import qualified  Language.Python.Version3.Parser     as P
import qualified  Language.Python.Common              as PC
import qualified  Language.Python.Common.Pretty       as T


pyParse :: String -> String -> Either String Prog
pyParse txt f = 
   case P.parseModule txt f of 
      Left  err   -> error (show err)
      Right (p,q) -> Right (convertPythonAST p)

checkPyParse :: FilePath -> IO ()
checkPyParse file = do
   s <- readFile file
   case pyParse s file of
      Left err -> print err
      Right p  -> print p

convertPythonAST :: PC.ModuleSpan -> Prog 
convertPythonAST (PC.Module prog) = map convertStmt prog 

convertStmt :: PC.StatementSpan -> Stmt 
convertStmt (PC.StmtExpr e _)                = error $ show e
convertStmt (PC.Assign a e _) = Assign (convertEIdent a []) (convertExpr e) 
convertStmt (PC.AnnotatedAssign _ (PC.Var v _) (Just e) _) = Assign [convertIdent v] (convertExpr e)
-- convertStmt (PC.AugmentedAssign _ op e _)  = undefined
convertStmt (PC.Conditional ((e,p):(s:_)) (sz:_) _)  = IfThen (convertExpr e) (convertStmt sz)
convertStmt rest                             = OtherS "Indeterminate statement"

convertExpr :: PC.ExprSpan -> Expr 
convertExpr (PC.Var v _)   = EVar (convertIdent v)
convertExpr (PC.Int i p _) = Lit (LInt i)
convertExpr (PC.Bool b _)  = Lit (LBool b)
convertExpr (PC.Strings x _) = polyConcat x ""
convertExpr (PC.BinaryOp op e1 e2 _) = Bin (convertBOp op) (convertExpr e1) (convertExpr e2) 
convertExpr (PC.UnaryOp op e _)      = Un (convertUOp op) (convertExpr e)
convertExpr (PC.List e _)            = Array (map convertExpr e)
convertExpr rest = OtherE "Indeterminate expression"

polyConcat :: [String] -> String -> Expr
polyConcat [] rest 
   | length rest == 1 = 
      let (x:_) = rest 
      in Lit (LChar x)
   | otherwise        = Lit (LStr rest)
polyConcat (x:xs) rest = polyConcat xs (x ++ rest)

convertIdent :: PC.IdentSpan -> String
convertIdent (PC.Ident s _) = s

convertEIdent :: [PC.ExprSpan] -> [Var] -> [Var] 
convertEIdent [] vars = reverse vars
convertEIdent (PC.Var v _ : rest) vars = convertEIdent rest (convertIdent v : vars)
convertEIdent (PC.Tuple [] _ : rest) vars = reverse vars
convertEIdent (PC.Tuple (PC.Var x _ : vs) _ : rest) vars = convertEIdent vs (convertIdent x : vars)
convertEIdent x _ = error $ show x

convertBOp :: PC.OpSpan -> BOp 
convertBOp (PC.Plus _) = Add 
convertBOp (PC.Minus _) = Sub 
convertBOp (PC.Multiply _) = Mul
convertBOp (PC.Divide _) = Div 
-- convertBOp (PC.FloorDivide _) = undefined 
-- convertBOp (PC.Exponent _) = Exp 
convertBOp (PC.Modulo _) = Mod 
convertBOp (PC.And _) = And
convertBOp (PC.Or _) = Or 
convertBOp (PC.LessThan _) = Less 
convertBOp (PC.GreaterThan _) = Greater 
convertBOp (PC.LessThanEquals _) = LessEq
convertBOp (PC.GreaterThanEquals _) = GreaterEq 
convertBOp (PC.Equality _) = Eq 
convertBOp (PC.NotEquals _) = NEq 
convertBOp rest = OtherB "Indeterminate binary operator"

convertUOp :: PC.OpSpan -> UOp 
convertUOp (PC.Not _) = Not 
convertUOp rest       = OtherU "Indeterminate unary operator"


-- Pretty-Printing:
---------------------------------------------------------------------

-- Will be used to add def main(): to beginning and main() and such.
printPython :: Prog -> String 
printPython = prettyPython ""

prettyPython :: String -> Prog -> String
prettyPython = foldl (\ text x -> text ++ printStmt x ++ "\n")

printStmt :: Stmt -> String
printStmt (Assign [v] e)        = v ++ " = " ++ printExpr e
printStmt (Assign (v:vs) e)     = v ++ " = " ++ printStmt (Assign vs e)
printStmt (Declare ty (v:vs) e) = 
   let (z:zs) = reverse (v:vs)
   in case e of 
      Nothing -> v ++ printDecl (Declare ty vs e)  
      Just e' -> v  ++ printDecl (Declare ty vs e) ++ " = " ++ printExpr e'
printStmt (IfThen e s)          = "if " ++ printExpr e ++ ":\n\t" ++ printStmt s 
printStmt (Output e)            = "print(" ++ printExpr e ++ ")"
printStmt (OtherS _)            = "Statement not yet implemented =("
printStmt (Block [])            = ""
printStmt (Block (s:ss))        = printStmt s ++ "\n" ++ printStmt (Block ss)
printStmt x                     = error $ show x

printDecl :: Stmt -> String 
printDecl (Declare _ [] _)     = ""
printDecl (Declare a (v:vs) e) = " = " ++ v ++ printDecl (Declare a vs e) 

printType :: Type -> String
printType TyInt  = "int"
printType TyBool = "bool"
printType TyStr  = "string"
printType Poly   = "<Type unknown>"
printType (TVar t) = "Type variable: " ++ show t 
printType (TyArr t) = "list[" ++ printType t ++ "]"
printType (OtherT _) = "Type not yet implemented =("

printExpr :: Expr -> String
printExpr (Lit (LInt n))      = show n
printExpr (Lit (LBool True))  = "True"
printExpr (Lit (LBool False)) = "False"
printExpr (Lit (LChar c))     = show c
printExpr (Lit (LStr s))      = show s -- think we need to add char as well
printExpr (EVar v)            = v
printExpr (Array a)           = "[" ++ reverse (printArray a "") ++ "]"
printExpr (Bin bop x y)       = printExpr x ++ printBop bop ++ printExpr y
printExpr (Un uop x)          = printUop uop ++ printExpr x
printExpr (OtherE _)          = "Expression not yet implemented =("

printArray :: [Expr] -> String -> String 
printArray []       str = ""
printArray [x]      str = reverse (printExpr x) ++ str
printArray (x:rest) str = printArray rest ("," ++ reverse (printExpr x) ++ str)

printString :: [Expr] -> String -> String 
printString []                     str = str 
printString ((Lit (LChar c)):rest) str = printString rest (c:str)
printString e _ = "Character expected in printString, but the following was found instead: " ++ printArray e ""

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
printBop (OtherB _) = "Binary operator not yet implemented =("

printUop :: UOp -> String 
printUop Not = "not "
printUop Neg = "-"
printUop (OtherU _) = "Unary operator not yet implemented =("

