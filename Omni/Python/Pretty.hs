

module Omni.Python.Pretty 
   ( 
      printPython 
   ) 
   where 


import            Omni.Data


-- Printing Python:
---------------------------------------------------------------------
printPython :: Prog -> Either PrettyError String
printPython = prettyPython ""

prettyPython :: String -> Prog -> Either PrettyError String
prettyPython text []       = Right (reverse (reverse "\nmain()\n" ++ text))
prettyPython text (x:rest) = do
   stmt <- printStmt 0 x
   prettyPython (reverse stmt ++ text) rest


-- Statements:
---------------------------------------------------------------------
tab :: Int -> String
tab i = concat (replicate i "\t")

printStmt :: Int -> Stmt -> Either PrettyError String
printStmt i (Assign [v] e)        = do
   expr <- printExpr e
   Right (tab i ++ v ++ " = " ++ expr)
printStmt i (Assign (v:vs) e)     = do
   stmt <- printStmt i (Assign vs e)
   Right (v ++ " = " ++ stmt)
printStmt i (AugAssign v aop e)   = do 
   expr <- printExpr e 
   aop  <- printAop aop
   Right (tab i ++ v ++ aop ++ expr)
printStmt i (Declare ty (v:vs) e) =
   let (z:zs) = reverse (v:vs)
   in case e of
      Nothing -> Right (tab i ++ v ++ printDecl (Declare ty vs e))
      Just e' -> do
         expr <- printExpr e'
         Right (tab i ++ v  ++ printDecl (Declare ty vs e) ++ " = " ++ expr)
printStmt i (IfThen e s)          = do
   expr <- printExpr e
   stmt <- printStmt (i+1) s
   Right (tab i ++ "if " ++ expr ++ ":\n" ++ stmt)
printStmt i (While e s)           = do
   expr <- printExpr e
   stmt <- printStmt (i+1) s
   Right (tab i ++ "while " ++ expr ++ ":\n" ++ stmt)
printStmt i (Block [])            = Right ""
printStmt i (Block [s])           = printStmt i s
printStmt i (Block (s:rest))      = do
   stmt1 <- printStmt i s
   stmt2 <- printStmt i (Block rest)
   Right (stmt1 ++ "\n" ++ stmt2)
printStmt _ (FuncDecl v a ty s)   = do
   stmt <- printStmt 1 s
   Right ("def " ++ v ++ "(" ++ reverse (printArgs a "") ++ "):\n" ++ stmt ++ "\n")
printStmt i (ExprStmt e)          = do 
   expr <- printExpr e
   Right (tab i ++ expr)
printStmt i (Return e)            =
   case e of
      Nothing -> Right (tab i ++ "return")
      Just x  -> do
         expr <- printExpr x
         Right (tab i ++ "return " ++ expr)
printStmt i (OtherS s)            = Left (BadStmt (OtherS s))
printStmt i x                     = error $ show x

printDecl :: Stmt -> String
printDecl (Declare _ [] _)     = ""
printDecl (Declare a (v:vs) e) = " = " ++ v ++ printDecl (Declare a vs e)
printDecl _                    = error "Unmatched pattern in printDecl"

printArg :: Args -> String  
printArg (Args _ ident) = show ident

printArgs :: [Args] -> String -> String
printArgs []       str = ""
printArgs [x]      str = reverse (printArg x) ++ str
printArgs (x:rest) str = printArgs rest ("," ++ reverse (printArg x) ++ str)


-- Types:
---------------------------------------------------------------------
printType :: Type -> Either PrettyError String
printType TyInt      = Right "int"
printType TyBool     = Right "bool"
printType TyStr      = Right "string"
printType Poly       = Right "poly"
printType (TVar t)   = Right ("Type variable: " ++ show t)
printType (TyArr t)  = do
   ty <- printType t
   Right ("list[" ++ ty ++ "]")
printType TyVoid     = Right ""
printType (OtherT t) = Left (BadType (OtherT t))
printType e          = Left (Misc ("Pattern not matched in printType: " ++ show e))


-- Expressions:
---------------------------------------------------------------------
printExpr :: Expr -> Either PrettyError String
printExpr (Lit l)             = printLit l
printExpr (Var v)             = Right v
printExpr (Array a)           = do
   arr <- printArray a ""
   Right ("[" ++ reverse arr ++ "]")
printExpr (Bin bop x y)       = do
   expr1 <- printExpr x
   expr2 <- printExpr y
   bop <- printBop bop
   Right (expr1 ++ bop ++ expr2)
printExpr (Un uop x)          = do
   uop <- printUop uop
   expr <- printExpr x
   Right (uop ++ expr)
printExpr (Call v e)          = do
   arr <- printArray e ""
   Right (concat v ++ "(" ++ reverse arr ++ ")")
printExpr (Output e)          = do
   expr <- printExpr e
   Right ("print(" ++ expr ++ ")")
printExpr (OtherE e)          = Left (BadExpr (OtherE e))

printLit :: Literal -> Either PrettyError String
printLit (Int n)      = Right (show n)
printLit (Float f)    = Right (show f)
printLit (Bool True)  = Right "True"
printLit (Bool False) = Right "False"
printLit (Char c)     = Right (show c)
printLit (Str s)      = Right s
printLit (Double d)   = Left (BadLit (Double d))
printLit (OtherL l)   = Left (BadLit (OtherL l))
-- Eventually will be used to catch all the bad conversions, but want to be alerted when pattern isn't matched for now
-- printLit rest         = Left (BadLit rest)

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
   Left (Misc ("Character expected in printString, but the following was found instead: " ++ arr))


-- Operators:
---------------------------------------------------------------------
printBop :: BOp -> Either PrettyError String
printBop Add         = Right " + "
printBop Sub         = Right " - "
printBop Mul         = Right " * "
printBop Div         = Right " / "
printBop Mod         = Right " % "
printBop Exp         = Right " ** "
printBop And         = Right " and "
printBop Or          = Right " or "
printBop Eq          = Right " == "
printBop NEq         = Right " != "
printBop Less        = Right " < "
printBop Greater     = Right " > "
printBop LessEq      = Right " <= "
printBop GreaterEq   = Right " >= "
printBop (OtherB b)  = Left (BadBop (OtherB b))
printBop e           = Left (Misc ("Pattern not matched in printBop: " ++ show e))

printUop :: UOp -> Either PrettyError String
printUop Not         = Right "not "
printUop Neg         = Right "-"
printUop (OtherU u)  = Left (BadUop (OtherU u))
printUop e           = Left (Misc ("Pattern not matched in printUop: " ++ show e))

printAop :: AOp -> Either PrettyError String 
printAop AddAssign   = Right " += "
printAop SubAssign   = Right " -= "
printAop (OtherA a)  = Left (BadAop (OtherA a))
printAop e           = Left (Misc ("Pattern not matched in printAop: " ++ show e))

