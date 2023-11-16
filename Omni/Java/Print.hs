

module Omni.Java.Print 
   (
      printJava
   )
   where 


import            Omni.Data 
import            Omni.Pretty


-- Printing Java:
---------------------------------------------------------------------
printJava :: String -> Prog -> Either PrettyError String
printJava name prog = do
   str <- prettyJava 2 "" prog
   Right ("public class " ++ name ++ " {\n\tpublic static void main(String[] args) {" ++ str ++ "\n\t}\n}")

prettyJava :: Int -> String -> Prog -> Either PrettyError String
prettyJava i str []        = Right (reverse str)
prettyJava i str (x:rest)  =
   case x of
      (While e s)      -> printStmt i (While e s) >>= \stmt -> 
         prettyJava i (reverse stmt ++ tab i ++ "\n" ++ str) rest
      (IfThen e s)     -> printStmt i (IfThen e s) >>= \stmt -> 
         prettyJava i (reverse stmt ++ tab i ++ "\n" ++ str) rest
      (IfElse e s1 s2) -> printStmt i (IfElse e s1 s2) >>= \stmt -> 
         prettyJava i (reverse stmt ++ tab i ++ "\n" ++ str) rest
      _                -> printStmt i x >>= \stmt -> 
         prettyJava i (";" ++ reverse stmt ++ tab i ++ "\n" ++ str) rest


-- Statements:
---------------------------------------------------------------------
tab :: Int -> String
tab i = concat (replicate i "\t")

printStmt :: Int -> Stmt -> Either PrettyError String
printStmt i (Assign [v] e)        = do
   expr <- printExpr e
   Right (v ++ " = " ++ expr)
printStmt i (Assign (v:vs) e)     = do
   stmt <- printStmt i (Assign vs e)
   Right (v ++ " = " ++ stmt)
printStmt i (Declare ty (v:vs) e) = do
   let (z:zs) = reverse (v:vs)
   ty' <- printType ty
   case e of
      Nothing -> Right (ty' ++ " " ++ v  ++ printDecl (Declare ty vs e) ++ printDeclAssign (Declare ty vs e))
      Just e' -> printExpr e' >>= \expr -> 
         Right (ty' ++ " " ++ v  ++ printDecl (Declare ty vs e) ++ printDeclAssign (Declare ty zs e) ++ " = " ++ expr)
printStmt i (IfThen e s)          = do
   expr <- printExpr e
   stmt <- printStmt (i+1) s
   Right ("if (" ++ expr ++ ") {" ++ stmt ++ "\n" ++ tab i ++ "}")
printStmt i (IfElse e s1 s2)      = do
   expr <- printExpr e
   stmt1 <- printStmt (i+1) s1
   case s2 of
      (IfElse {}) -> printStmt i s2 >>= \stmt2 -> 
         Right ("if (" ++ expr ++ ") {" ++ stmt1 ++ "\n" ++ tab i ++ "} else " ++ stmt2)
      _ -> printStmt (i+1) s2 >>= \stmt2 -> 
         Right ("if (" ++ expr ++ ") {" ++ stmt1 ++ "\n" ++ tab i ++ "} else {" ++ stmt2 ++ "\n" ++ tab i ++ "}")
printStmt i (While e s)           = do
   expr <- printExpr e
   stmt <- printStmt (i+1) s
   Right ("while (" ++ expr ++ ") {" ++ stmt ++ "\n" ++ tab i ++ "}")
printStmt i (Block s)             = prettyJava i "" s
printStmt i (ExprStmt e)          = do 
   expr <- printExpr e
   Right expr
printStmt i (OtherS s)            = Left (BadStmt (OtherS s))
printStmt _ x                     = Left (Misc ("Pattern not matched in printStmt: " ++ show x))

printDecl :: Stmt -> String
printDecl (Declare _ [] _)     = ""
printDecl (Declare a (v:vs) e) = ',' : v ++ printDecl (Declare a vs e)
printDecl _                    = error "Unmatched pattern in printDecl"

printDeclAssign :: Stmt -> String
printDeclAssign (Declare _ [] _)     = ""
printDeclAssign (Declare a (v:vs) e) =
   case e of
      Nothing -> ""
      Just x  -> " = " ++ v ++ printDeclAssign (Declare a vs e)
printDeclAssign _                    = error "Unmatched pattern in printDeclAssign"


-- Types:
---------------------------------------------------------------------
printType :: Type -> Either PrettyError String
printType TyInt      = Right "int"
printType TyFloat    = Right "float"
printType TyDouble   = Right "double"
printType TyBool     = Right "boolean"
printType TyChar     = Right "char"
printType TyStr      = Right "String"
printType (TyArr x)  = do
   ty <- printType x
   Right (ty ++ "[]")
printType Poly       = Right "Poly"
printType (TVar t)   = Right ("Type variable: " ++ show t)
printType TyVoid     = Right "void"
printType (OtherT t) = Left (BadType (OtherT t))


-- Expressions:
---------------------------------------------------------------------
printExpr :: Expr -> Either PrettyError String
printExpr (Lit l)       = printLit l
printExpr (Var v)       = Right v
printExpr (Array ((Lit (Char c)):cs)) = do
   str <- printString (Lit (Char c):cs) ""
   Right (reverse str)
printExpr (Array arr)   = do
   arr <- printArray arr ""
   Right ("{" ++ reverse arr ++ "}")
printExpr (Bin bop x y) = do
   expr1 <- printExpr x
   expr2 <- printExpr y
   bop   <- printBop bop
   Right (expr1 ++ bop ++ expr2)
printExpr (Un uop x)    = do
   expr <- printExpr x
   uop  <- printUop uop
   Right (uop ++ expr)
printExpr (Output e)    = do 
   expr <- printExpr e 
   Right ("System.out.println(" ++ expr ++ ")")
printExpr (OtherE e)    = Left (BadExpr (OtherE e))
printExpr e             = Left (Misc ("Pattern not matched in printExpr: " ++ show e))

printLit :: Literal -> Either PrettyError String
printLit (Int n)       = Right (show n)
printLit (Float f)     = Right (show f)
printLit (Double d)    = Right (show d)
printLit (Bool True)   = Right "true"
printLit (Bool False)  = Right "false"
printLit (Char c)      = Right (show c)
printLit (Str s)       = Right s
printLit (OtherL l)    = Left (BadLit (OtherL l))
-- printLit e            = Left (Misc ("Pattern not matched in printLit: " ++ show e))

printArray :: [Expr] -> String -> Either PrettyError String
printArray []       str = Right ""
printArray [x]      str = do
   expr <- printExpr x
   Right (reverse expr ++ str)
printArray (x:rest) str = do
   expr <- printExpr x
   printArray rest ("," ++ reverse expr ++ str)

printString :: [Expr] -> String -> Either PrettyError String
printString []                    str = Right str
printString ((Lit (Char c)):rest) str = printString rest (c:str)
printString e                     _   = do
   arr <- printArray e ""
   Left (Misc ("Character/string expected in printString, but the following was found instead: " ++ arr))


-- Operators:
---------------------------------------------------------------------
printBop :: BOp -> Either PrettyError String
printBop Add         = Right " + "
printBop Sub         = Right " - "
printBop Mul         = Right " * "
printBop Div         = Right " / "
printBop Mod         = Right " % "
printBop And         = Right " && "
printBop Or          = Right " || "
printBop Eq          = Right " == "
printBop NEq         = Right " != "
printBop Less        = Right " < "
printBop Greater     = Right " > "
printBop LessEq      = Right " <= "
printBop GreaterEq   = Right " >= "
printBop (OtherB b)  = Left (BadBop (OtherB b))
printBop e           = Left (Misc ("Pattern not matched in printBop: " ++ show e))

printUop :: UOp -> Either PrettyError String
printUop Not         = Right "!"
printUop Neg         = Right "-"
printUop (OtherU u)  = Left (BadUop (OtherU u))
printUop e           = Left (Misc ("Pattern not matched in printUop: " ++ show e))

printAop :: AOp -> Either PrettyError String 
printAop AddAssign   = Right "+="
printAop SubAssign   = Right "-="
printAop (OtherA a)  = Left (BadAop (OtherA a))
printAop e           = Left (Misc ("Pattern not matched in printAop: " ++ show e))

