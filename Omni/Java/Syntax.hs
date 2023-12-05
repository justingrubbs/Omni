

module Omni.Java.Syntax
   (
      javaParse, checkJavaParse
   )
   where


import            Omni.Data
import qualified  Language.Java.Parser    as P
import qualified  Language.Java.Syntax    as S
import            Language.Java.Pretty


-- Parsing Java Compilation Unit:
---------------------------------------------------------------------
javaParse :: String -> Either Error Prog
javaParse txt =
   case P.parser P.compilationUnit txt of
      Left  err -> Left (ParseError (Generic $ show err))
      Right x   -> Right (convertJavaAST x)
      -- Right x   ->  error $ show x

modifyProg :: Prog -> Prog -> Prog
modifyProg new []       = new
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


-- Converting to Omni AST:
---------------------------------------------------------------------
convertJavaAST :: S.CompilationUnit -> Prog
convertJavaAST (S.CompilationUnit _ _ x) = map convertTypeDecl x


-- Large declarations:
---------------------------------------------------------------------
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


-- Functions:
---------------------------------------------------------------------
convertMemberDecl :: S.MemberDecl -> Stmt
convertMemberDecl (S.MethodDecl _ _ ty (S.Ident "main") p _ _ s) 
   = FunDecl "main" [] TyVoid (convertMethodBody s)
convertMemberDecl (S.MethodDecl _ _ ty v                p _ _ s) 
   = case ty of 
      Nothing -> FunDecl (convertIdent v) (map convertFormalParam p) TyVoid (convertMethodBody s)
      Just x  -> FunDecl (convertIdent v) (map convertFormalParam p) (convertType x) (convertMethodBody s)
convertMemberDecl rest 
   = OtherS (prettyPrint rest)

convertFormalParam :: S.FormalParam -> Args 
convertFormalParam (S.FormalParam _ ty _ (S.VarId v))        = Args (convertType ty) (convertIdent v)
convertFormalParam (S.FormalParam x ty y (S.VarDeclArray v)) = convertFormalParam (S.FormalParam x ty y v)

convertMethodBody :: S.MethodBody -> Stmt
convertMethodBody (S.MethodBody (Just x)) = convertBlock x
convertMethodBody (S.MethodBody Nothing)  = Block []


-- Statements:
---------------------------------------------------------------------
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
convertStmt (S.ExpStmt (S.Assign lhs aop e))      =
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


-- Types:
---------------------------------------------------------------------
convertType :: S.Type -> Type
convertType (S.PrimType x) = convertPrimType x
convertType (S.RefType  x) = convertRefType  x

convertPrimType :: S.PrimType -> Type
convertPrimType S.IntT     = TyInt
convertPrimType S.BooleanT = TyBool
convertPrimType S.CharT    = TyChar
convertPrimType S.FloatT   = TyFloat
convertPrimType S.DoubleT  = TyDouble
convertPrimType rest       = OtherT (prettyPrint rest)

convertRefType :: S.RefType -> Type
convertRefType (S.ArrayType t) =
   case convertType t of
      TyChar -> TyStr
      x      -> TyArr x
convertRefType (S.ClassRefType (S.ClassType []))             = undefined
convertRefType (S.ClassRefType (S.ClassType ((_,ct:_) : _))) = convertTypeArg ct
convertRefType (S.ClassRefType (S.ClassType [(_, [])]))      = TyStr -- I have absolutely no idea what's going on here
convertRefType (S.ClassRefType (S.ClassType ((_, []):_:_)))  = error "HOUGROUGWRJIGOW"

convertTypeArg :: S.TypeArgument -> Type
convertTypeArg (S.ActualType t) = convertRefType t
convertTypeArg (S.Wildcard e)   = OtherT (prettyPrint (S.Wildcard e))


-- Variables and Identifiers:
---------------------------------------------------------------------
convertIdent :: S.Ident -> String
convertIdent (S.Ident str) = str

convertVarDecl :: S.VarDecl -> Type -> Stmt
convertVarDecl (S.VarDecl (S.VarId x) y) ty        =
   case y of
      Nothing -> Declare ty [convertIdent x] Nothing
      Just z  -> Declare ty [convertIdent x] (Just (convertVarInit z))
convertVarDecl (S.VarDecl (S.VarDeclArray x) y) ty =
   let x' = checkVarDecl x
   in case y of
         Nothing -> Declare ty [x'] Nothing
         Just z  -> do 
            let z' = convertVarInit z 
            case z' of 
               Lit Null -> Block []
               _ -> Declare ty [x'] (Just z')

convertVarDecl2 :: S.VarDecl -> Ident
convertVarDecl2 (S.VarDecl (S.VarId x) y)        = convertIdent x
convertVarDecl2 (S.VarDecl (S.VarDeclArray x) y) = checkVarDecl x

checkVarDecl :: S.VarDeclId -> String
checkVarDecl (S.VarDeclArray x) =
   case x of
      (S.VarDeclArray y) -> checkVarDecl y
      (S.VarId y)        -> convertIdent y
checkVarDecl _                  = undefined

convertVarInit :: S.VarInit -> Expr
convertVarInit (S.InitExp exp)   = convertExpr exp
convertVarInit (S.InitArray arr) = convertArrayInit arr

convertArrayInit :: S.ArrayInit -> Expr 
convertArrayInit (S.ArrayInit a) = Array (map convertVarInit a)


-- Expressions:
---------------------------------------------------------------------
convertExpr :: S.Exp -> Expr
convertExpr (S.Lit x)                     = Lit (convertLit x)
convertExpr (S.ExpName (S.Name (x:rest))) = Var (convertIdent x)
convertExpr (S.ArrayCreateInit _ _ arr)   = convertArrayInit arr
convertExpr (S.BinOp e1 op e2)            = Bin (convertBinOp op) (convertExpr e1) (convertExpr e2)
convertExpr (S.PreIncrement e)            = Un Incr (convertExpr e)
convertExpr (S.PostIncrement e)           = Un Incr (convertExpr e)
convertExpr (S.PreDecrement e)            = Un Decr (convertExpr e)
convertExpr (S.PostDecrement e)           = Un Decr (convertExpr e)
convertExpr (S.PreMinus e)                = Un Neg (convertExpr e)
convertExpr (S.PreNot e)                  = Un Not (convertExpr e)
convertExpr (S.Assign lhs op e)           = convertExpr e
convertExpr (S.MethodInv inv)             = convertMethodInvocation inv
convertExpr rest                          = error $ show rest

convertLit :: S.Literal -> Literal
convertLit (S.Int i)     = Int i
convertLit (S.Boolean b) = Bool b
convertLit (S.Char c)    = Char c
convertLit (S.String s)  = Str s
convertLit S.Null        = Null
convertLit rest          = OtherL (prettyPrint rest)

convertMethodInvocation :: S.MethodInvocation -> Expr 
convertMethodInvocation (S.MethodCall (S.Name v) e) = 
   let v' = map convertIdent v 
   in if v' == ["System","out","println"]
      then 
         case e of 
            [x] -> Output (convertExpr x)
            _   -> Output (Array (map convertExpr e))
      else Call (map convertIdent v) (map convertExpr e) 
convertMethodInvocation rest                        = OtherE (prettyPrint rest)


-- Operators:
---------------------------------------------------------------------
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

