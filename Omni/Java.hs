{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Omni.Java
   (
      javaParse, checkJavaParse, printJava
   )
   where


import            Omni.Data
import            Omni.Pretty
import qualified  Language.Java.Parser    as P
import qualified  Language.Java.Syntax    as S
import            Language.Java.Pretty


javaParse :: String -> Either String Prog
javaParse txt =
   case P.parser P.compilationUnit txt of
      Left  err -> error (show err)
      Right x   -> Right (convertJavaAST x)
      -- Temporary: =)
      -- Right x   ->  error $ show x

modifyProg :: Prog -> Prog -> Prog
modifyProg new [] = new
modifyProg new (x:rest) =
   case x of
      Block y -> modifyProg new y
      y       -> modifyProg (y : new) rest

checkJavaParse :: FilePath -> IO ()
checkJavaParse file = do
   s <- readFile file
   case javaParse s of
      Left err -> print err
      Right p  -> print p

convertJavaAST :: S.CompilationUnit -> Prog
convertJavaAST (S.CompilationUnit _ _ x) = map convertTypeDecl x

convertTypeDecl :: S.TypeDecl -> Stmt
convertTypeDecl (S.ClassTypeDecl x) = convertClassDecl x
convertTypeDecl interfaceTypeDecl   = OtherS (prettyPrint interfaceTypeDecl)

convertClassDecl :: S.ClassDecl -> Stmt
convertClassDecl (S.ClassDecl _ _ _ _ _ x) = convertClassBody x
convertClassDecl enumDecl                  = OtherS (prettyPrint enumDecl)

convertClassBody :: S.ClassBody -> Stmt
convertClassBody (S.ClassBody x) = Block (map convertDecl x)

convertDecl :: S.Decl -> Stmt
convertDecl (S.MemberDecl x) = convertMemberDecl x
convertDecl initDecl         = OtherS (prettyPrint initDecl)


-- https://www.mygreatlearning.com/blog/polymorphism-in-java/
convertMemberDecl :: S.MemberDecl -> Stmt -- once implementing functions, change this
convertMemberDecl (S.MethodDecl _ _ ty (S.Ident "main") p _ _ s) = FuncDecl "main" [] TyVoid (convertMethodBody s)
convertMemberDecl (S.MethodDecl _ _ ty v                p _ _ s) = 
   case ty of 
      Nothing -> FuncDecl (convertIdent v) (map convertFormalParam p) TyVoid (convertMethodBody s)
      Just x  -> FuncDecl (convertIdent v) (map convertFormalParam p) (convertType x) (convertMethodBody s)
convertMemberDecl rest = OtherS (prettyPrint rest)

convertFormalParam :: S.FormalParam -> Expr 
convertFormalParam (S.FormalParam _ ty _ (S.VarId v))        = Args (convertType ty) (convertIdent v)
convertFormalParam (S.FormalParam x ty y (S.VarDeclArray v)) = convertFormalParam (S.FormalParam x ty y v)

convertMethodBody :: S.MethodBody -> Stmt
convertMethodBody (S.MethodBody (Just x)) = convertBlock x
convertMethodBody (S.MethodBody Nothing)  = Block []

convertBlock :: S.Block -> Stmt
convertBlock (S.Block x) = Block (map convertBlockStmt x)

convertBlockStmt :: S.BlockStmt -> Stmt
convertBlockStmt (S.BlockStmt stmt)  = convertStmt stmt
convertBlockStmt (S.LocalVars _ t x) =
   let (z:_) = reverse x
   in case convertVarDecl z (convertType t) of
      Declare _ _ e -> Declare (convertType t) (map convertVarDecl2 x) e
      e             -> error ("Pattern unmatched in convertBlockStmt: " ++ show e)
convertBlockStmt localClass          = OtherS (prettyPrint localClass)

convertStmt :: S.Stmt -> Stmt
convertStmt (S.StmtBlock b)                       = convertBlock b
convertStmt (S.ExpStmt (S.Assign lhs aop e)) =
   case lhs of
      S.NameLhs (S.Name x) -> 
         case aop of 
            S.EqualA -> Assign (map convertIdent x) (convertExpr e)
            _        -> 
               let (x':_) = x 
               in AugAssign (convertIdent x') (convertAop aop) (convertExpr e)
      x                    -> OtherS (prettyPrint lhs)
convertStmt (S.IfThen e s)                        = IfThen (convertExpr e) (convertStmt s)
convertStmt (S.IfThenElse e s1 s2)                = IfElse (convertExpr e) (convertStmt s1) (convertStmt s2)
convertStmt (S.While e s)                         = While (convertExpr e) (convertStmt s)
convertStmt (S.ExpStmt e)                         = ExprStmt (convertExpr e)
convertStmt (S.Return e)                          = 
   case e of 
      Nothing -> Return Nothing
      Just x  -> Return (Just (convertExpr x))
convertStmt other                                 = OtherS (prettyPrint other)

convertType :: S.Type -> Type
convertType (S.PrimType x) = convertPrimType x
convertType (S.RefType  x) = convertRefType  x

convertPrimType :: S.PrimType -> Type
convertPrimType S.IntT     = TyInt
convertPrimType S.BooleanT = TyBool
convertPrimType S.CharT    = TyChar
convertPrimType rest       = OtherT (prettyPrint rest)

convertRefType :: S.RefType -> Type
convertRefType (S.ArrayType t)    =
   case convertType t of
      TyChar -> TyStr
      x      -> TyArr x
convertRefType (S.ClassRefType (S.ClassType []))             = undefined
convertRefType (S.ClassRefType (S.ClassType ((_,ct:_) : _))) = convertTypeArg ct
convertRefType (S.ClassRefType (S.ClassType [(_, [])]))      = TyStr -- I'm not sure what's happening here really
convertRefType (S.ClassRefType (S.ClassType ((_, []):_:_)))  = error "HOUGROUGWRJIGOW"

convertTypeArg :: S.TypeArgument -> Type
convertTypeArg (S.ActualType t) = convertRefType t
convertTypeArg (S.Wildcard e)   = OtherT (prettyPrint (S.Wildcard e))

convertVarDecl :: S.VarDecl -> Type -> Stmt
convertVarDecl (S.VarDecl (S.VarId x) y) ty =
   case y of
      Nothing -> Declare ty [convertIdent x] Nothing
      Just z  -> Declare ty [convertIdent x] (Just (convertVarInit z))
convertVarDecl (S.VarDecl (S.VarDeclArray x) y) ty =
   let x' = checkVarDecl x
   in case y of
         Nothing -> Declare ty [x'] Nothing
         Just z  -> Declare ty [x'] (Just (convertVarInit z))

convertVarDecl2 :: S.VarDecl -> Ident
convertVarDecl2 (S.VarDecl (S.VarId x) y)        = convertIdent x
convertVarDecl2 (S.VarDecl (S.VarDeclArray x) y) = checkVarDecl x

checkVarDecl :: S.VarDeclId -> String
checkVarDecl (S.VarDeclArray x) =
   case x of
      (S.VarDeclArray y) -> checkVarDecl y
      (S.VarId y)        -> convertIdent y
checkVarDecl _ = undefined

convertIdent :: S.Ident -> String
convertIdent (S.Ident str) = str

convertVarInit :: S.VarInit -> Expr
convertVarInit (S.InitExp exp)               = convertExpr exp
convertVarInit (S.InitArray (S.ArrayInit x)) = Array (map convertVarInit x)

convertExpr :: S.Exp -> Expr
convertExpr (S.Lit x)                     = Lit (convertLit x)

   -- I think S.Name has a list because of stuff like `int x,y,z = 3;`   !!!
   -- Not dealing with that right now, so just ignoring the rest of them !!!
convertExpr (S.ExpName (S.Name (x:rest))) = Var (convertIdent x)
convertExpr (S.BinOp e1 op e2)            = Bin (convertBinOp op) (convertExpr e1) (convertExpr e2)
-- convertExpr (unary shit) 
convertExpr (S.Assign lhs op e)           = convertExpr e
convertExpr (S.MethodInv inv)             = convertMethodInvocation inv
convertExpr rest                          = error $ show rest

convertMethodInvocation :: S.MethodInvocation -> Expr 
convertMethodInvocation (S.MethodCall (S.Name v) e) = 
   let v' = map convertIdent v 
   in if v' == ["System","out","println"]
      then 
         case e of 
            [x] -> Output (convertExpr x)
            _   -> Output (Array (map convertExpr e))
      else Call (map convertIdent v) (map convertExpr e) 
convertMethodInvocation rest = OtherE (prettyPrint rest)

convertLit :: S.Literal -> Literal
convertLit (S.Int i)     = LInt i
convertLit (S.Boolean b) = LBool b
convertLit (S.Char c)    = LChar c
convertLit (S.String s)  = LStr s
convertLit rest          = OtherL (prettyPrint rest)

convertBinOp :: S.Op -> BOp
convertBinOp S.Mult   = Mul
convertBinOp S.Div    = Div
convertBinOp S.Add    = Add
convertBinOp S.Sub    = Sub
convertBinOp S.Rem    = Mod
convertBinOp S.And    = And
convertBinOp S.Or     = Or
convertBinOp S.Equal  = Eq
convertBinOp S.NotEq  = NEq
convertBinOp S.GThan  = Greater
convertBinOp S.LThan  = Less
convertBinOp S.GThanE = GreaterEq
convertBinOp S.LThanE = LessEq
convertBinOp rest     = OtherB (prettyPrint rest)

convertAop :: S.AssignOp -> AOp 
convertAop S.AddA   = AddAssign
convertAop S.SubA   = SubAssign 
convertAop S.MultA  = MulAssign 
convertAop S.DivA   = DivAssign 
convertAop rest     = OtherA (prettyPrint rest) 

-- Pretty-Printing:
---------------------------------------------------------------------

printJava :: String -> Prog -> Either PrettyError String
printJava name prog = do
   str <- prettyJava 2 "" prog
   Right ("public class " ++ name ++ " {\n\tpublic static void main(String[] args) {" ++ str ++ "\n\t}\n}")

prettyJava :: Int -> String -> Prog -> Either PrettyError String
prettyJava i str []        = Right (reverse str)
prettyJava i str (x:rest)  =
   case x of
      (While e s)      -> printStmt i (While e s) >>= \stmt
         -> prettyJava i (reverse stmt ++ tab i ++ "\n" ++ str) rest
      (IfThen e s)     -> printStmt i (IfThen e s) >>= \stmt
         -> prettyJava i (reverse stmt ++ tab i ++ "\n" ++ str) rest
      (IfElse e s1 s2) -> printStmt i (IfElse e s1 s2) >>= \stmt
         -> prettyJava i (reverse stmt ++ tab i ++ "\n" ++ str) rest
      x                -> printStmt i x >>= \stmt
         -> prettyJava i (";" ++ reverse stmt ++ tab i ++ "\n" ++ str) rest

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
      Just e' -> printExpr e' >>= \expr
         -> Right (ty' ++ " " ++ v  ++ printDecl (Declare ty vs e) ++ printDeclAssign (Declare ty zs e) ++ " = " ++ expr)
-- printStmt i (Output e)            = do
--    expr <- printExpr e
--    Right ("System.out.println(" ++ expr ++ ")")
printStmt i (IfThen e s)          = do
   expr <- printExpr e
   stmt <- printStmt (i+1) s
   Right ("if (" ++ expr ++ ") {" ++ stmt ++ "\n" ++ tab i ++ "}")
printStmt i (IfElse e s1 s2)      = do
   expr <- printExpr e
   stmt1 <- printStmt (i+1) s1
   case s2 of
      (IfElse {}) -> printStmt i s2 >>= \stmt2
         -> Right ("if (" ++ expr ++ ") {" ++ stmt1 ++ "\n" ++ tab i ++ "} else " ++ stmt2)
      _ -> printStmt (i+1) s2 >>= \stmt2
         -> Right ("if (" ++ expr ++ ") {" ++ stmt1 ++ "\n" ++ tab i ++ "} else {" ++ stmt2 ++ "\n" ++ tab i ++ "}")
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
printDecl _ = error "Unmatched pattern in printDecl"

printDeclAssign :: Stmt -> String
printDeclAssign (Declare _ [] _)     = ""
printDeclAssign (Declare a (v:vs) e) =
   case e of
      Nothing -> ""
      Just x  -> " = " ++ v ++ printDeclAssign (Declare a vs e)
printDeclAssign _ = error "Unmatched pattern in printDeclAssign"

printType :: Type -> Either PrettyError String
printType TyInt      = Right "int"
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

printExpr :: Expr -> Either PrettyError String
printExpr (Lit l)       = printLit l
printExpr (Var v)       = Right v
printExpr (Array ((Lit (LChar c)):cs)) = do
   str <- printString (Lit (LChar c):cs) ""
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
printLit (LInt n)       = Right (show n)
printLit (LBool True)   = Right "true"
printLit (LBool False)  = Right "false"
printLit (LChar c)      = Right (show c)
printLit (LStr s)       = Right s
printLit (OtherL l)     = Left (BadLit (OtherL l))
-- printLit e              = Left (Misc ("Pattern not matched in printLit: " ++ show e))

printArray :: [Expr] -> String -> Either PrettyError String
printArray []       str = Right ""
printArray [x]      str = do
   expr <- printExpr x
   Right (reverse expr ++ str)
printArray (x:rest) str = do
   expr <- printExpr x
   printArray rest ("," ++ reverse expr ++ str)

printString :: [Expr] -> String -> Either PrettyError String
printString []                     str = Right str
printString ((Lit (LChar c)):rest) str = printString rest (c:str)
printString e _ = do
   arr <- printArray e ""
   Left (Misc ("Character/string expected in printString, but the following was found instead: " ++ arr))

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

