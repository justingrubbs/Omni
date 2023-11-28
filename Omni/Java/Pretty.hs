

module Omni.Java.Pretty 
   (
      printJava
   )
   where 


import            Omni.Data 


-- Printing Java:
---------------------------------------------------------------------
printJava :: String -> Prog -> Either PrettyError String
printJava name prog = do
   str <- prettyJava 1 "" prog
         -- REMOVE PACKAGE THING ONCE PROJECT DONE
   Right ("package TestFiles;" ++ "\npublic class " ++ name ++ " {" ++ str ++ "\n}")

prettyJava :: Int -> String -> Prog -> Either PrettyError String
prettyJava i str []        = Right $ reverse str
prettyJava i str (x:rest)  =
   case x of
      (While e s)      -> printStmt i (While e s) >>= \stmt -> 
         prettyJava i (reverse stmt ++ tab i ++ "\n" ++ str) rest
      (IfThen e s)     -> printStmt i (IfThen e s) >>= \stmt -> 
         prettyJava i (reverse stmt ++ tab i ++ "\n" ++ str) rest
      (IfElse e s1 s2) -> printStmt i (IfElse e s1 s2) >>= \stmt -> 
         prettyJava i (reverse stmt ++ tab i ++ "\n" ++ str) rest
      (FuncDecl v args ty s) -> printStmt i (FuncDecl v args ty s) >>= \stmt -> 
         prettyJava i (reverse stmt ++ tab i ++ "\n" ++ str) rest
      (ExprStmt (Call ["main"] _)) -> prettyJava i str rest  -- bandaid
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
printStmt i (AugAssign v aop e)   = do 
   expr <- printExpr e 
   aop <- printAop aop 
   Right $ v ++ aop ++ expr
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
printStmt i (FuncDecl ident args ty s) = 
   case ident of 
      "main" -> 
         case args of 
            [] -> do 
               s <- printStmt (i+1) s 
               Right $ "public static void " ++ ident ++ "(String[] args) {" ++ s ++ "\n" ++ tab i ++ "}"
            -- Actually quite weird, not sure how I want this to be handled. 
            _  -> undefined
      _      -> do 
         args <- printArgs args ""
         ty <- printType ty 
         s <- printStmt (i+1) s 
         Right ("public static " ++ ty ++ " " ++ ident ++ "(" ++ args ++ ") {" ++ s ++ "\n" ++ tab i ++ "}")
printStmt i (ExprStmt e)          = do 
   expr <- printExpr e
   Right expr
printStmt i (Return e)            = 
   case e of 
      Just x  -> do 
         expr <- printExpr x 
         Right $ "return " ++ expr
      Nothing -> Right "return"
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

printArg :: Args -> Either PrettyError String   
printArg (Args ty ident) = do 
   ty <- printType ty 
   Right (ty ++ " " ++ ident)

printArgs :: [Args] -> String -> Either PrettyError String 
printArgs []       str = Right ""
printArgs [x]      str = do 
   arg <- printArg x 
   Right $ reverse (reverse arg ++ str)
printArgs (x:rest) str = do 
   arg <- printArg x 
   printArgs rest (" ," ++ reverse arg ++ str)


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
printExpr (ArrayA a ty) = do 
   arr <- printArray a ""
   ty' <- printType ty
   Right $ "new " ++ ty' ++ " {" ++ reverse arr ++ "}"
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
printExpr (Call v e)    = 
   case concat v of 
      "main" -> 
         if null e
         then Right "" 
         else undefined  -- Have not determined what to do whenever a non-main-esque function is defined as main
      v'     -> do 
         arr <- printArray e ""
         Right (concat v ++ "(" ++ reverse arr ++ ")")
printExpr (OtherE e)    = Left (BadExpr (OtherE e))
-- printExpr e             = Left (Misc ("Pattern not matched in printExpr: " ++ show e))

printLit :: Literal -> Either PrettyError String
printLit (Int n)       = Right (show n)
printLit (Float f)     = Right (show f)
printLit (Double d)    = Right (show d)
printLit (Bool True)   = Right "true"
printLit (Bool False)  = Right "false"
printLit (Char c)      = Right $ show "sfioewofjoejf'ej'fiwjeifjwe'fj'jaepi jfpdszjc'xjdj'cjv"
printLit (Str [s])     = Right $ show "sfioewofjoejf'ej'fiwjeifjwe'fj'jaepi jfpdszjc'xjdj'cjv"
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
printAop AddAssign   = Right " += "
printAop SubAssign   = Right " -= "
printAop (OtherA a)  = Left (BadAop (OtherA a))
printAop e           = Left (Misc ("Pattern not matched in printAop: " ++ show e))

