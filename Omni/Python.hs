-- {-# LANGUAGE RankNTypes #-}

module Omni.Python
   (
      pyParse, printPython, checkPyParse
   )
   where


import            Omni.Data
import            Omni.Pretty
import qualified  Language.Python.Version3.Parser     as P
import qualified  Language.Python.Common              as PC
import qualified  Language.Python.Common.Pretty       as T


pyParse :: String -> String -> Either String Prog
pyParse txt f =
   case P.parseModule txt f of
      Left  err   -> error (show err)
      Right (p,_) -> Right (convertPythonAST p)
      -- Right (p,_) -> error $ show p

checkPyParse :: FilePath -> IO ()
checkPyParse file = do
   s <- readFile file
   case pyParse s file of
      Left err -> print err
      Right p  -> print p

convertPythonAST :: PC.ModuleSpan -> Prog
convertPythonAST (PC.Module prog) = map convertStmt prog

convertStmt :: PC.StatementSpan -> Stmt
convertStmt (PC.StmtExpr e _)                = ExprStmt (convertExpr e)
convertStmt (PC.Assign a e _)                = Assign (convertEIdent a []) (convertExpr e)
convertStmt (PC.AnnotatedAssign _ (PC.Var v _) (Just e) _)  = Assign [convertIdent v] (convertExpr e)
convertStmt (PC.AugmentedAssign (PC.Var v _) op e _)        = AugAssign (convertIdent v) (convertAOp op) (convertExpr e)
convertStmt (PC.Conditional [(e,s)] [] _)    = IfThen (convertExpr e) (Block (map convertStmt s))

convertStmt (PC.Conditional [(e,s)] x y) = IfElse (convertExpr e) (Block (map convertStmt s)) (Block (map convertStmt x))
convertStmt (PC.Conditional ((e,s):sz) x y) =
   IfElse (convertExpr e) (Block (map convertStmt s)) (convertStmt (PC.Conditional sz x y))
convertStmt (PC.While e s [] _)              = While  (convertExpr e) (Block (map convertStmt s))
convertStmt (PC.Fun v para _ (s:_) _)        = FuncDecl (EVar (convertIdent v)) (map convertParam para) Poly (convertStmt s)
convertStmt (PC.Return e _)                  =
   case e of
      Nothing  -> Return Nothing
      Just exp -> Return (Just (convertExpr exp))
convertStmt rest                             = OtherS (show rest)

convertParam :: PC.ParameterSpan -> String
convertParam (PC.Param v _ _ _)  = convertIdent v
convertParam _                   = undefined

convertExpr :: PC.ExprSpan -> Expr
convertExpr (PC.Var v _)   = EVar (convertIdent v)
convertExpr (PC.Int i p _) = Lit (LInt i)
convertExpr (PC.Bool b _)  = Lit (LBool b)
convertExpr (PC.Strings x _)         = polyConcat x ""
convertExpr (PC.BinaryOp op e1 e2 _) = Bin (convertBOp op) (convertExpr e1) (convertExpr e2)
convertExpr (PC.UnaryOp op e _)      = Un (convertUOp op) (convertExpr e)
convertExpr (PC.List e _)            = Array (map convertExpr e)
convertExpr (PC.Paren e _)           = convertExpr e
convertExpr (PC.Call e a _)          = Call (convertExpr e) (map convertArg a)
convertExpr rest           = OtherE (T.prettyText rest)

convertArg :: PC.ArgumentSpan -> Expr
convertArg (PC.ArgExpr e _) = convertExpr e
convertArg rest             = OtherE (T.prettyText rest)

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
convertBOp (PC.Exponent _) = Exp
convertBOp (PC.Modulo _) = Mod
convertBOp (PC.And _) = And
convertBOp (PC.Or _) = Or
convertBOp (PC.LessThan _) = Less
convertBOp (PC.GreaterThan _) = Greater
convertBOp (PC.LessThanEquals _) = LessEq
convertBOp (PC.GreaterThanEquals _) = GreaterEq
convertBOp (PC.Equality _) = Eq
convertBOp (PC.NotEquals _) = NEq
convertBOp rest = OtherB (T.prettyText rest)

convertUOp :: PC.OpSpan -> UOp
convertUOp (PC.Not _) = Not
convertUOp rest       = OtherU (T.prettyText rest)

convertAOp :: PC.AssignOpSpan -> AOp
convertAOp (PC.PlusAssign _)  = AddAssign
convertAOp (PC.MinusAssign _) = SubAssign
convertAOp rest               = OtherA (T.prettyText rest)


-- Pretty-Printing:
---------------------------------------------------------------------

-- Will be used to add def main(): to beginning and main() and such.
printPython :: Prog -> Either PrettyError String
printPython = prettyPython ""

prettyPython :: String -> Prog -> Either PrettyError String
prettyPython text []       = Right (reverse text)
prettyPython text (x:rest) = do
   stmt <- printStmt 0 x
   prettyPython (reverse stmt ++ text) rest

tab :: Int -> String
tab i = concat (replicate i "\t")

printStmt :: Int -> Stmt -> Either PrettyError String
printStmt i (Assign [v] e)        = do
   expr <- printExpr e
   Right (v ++ " = " ++ expr)
printStmt i (Assign (v:vs) e)     = do
   stmt <- printStmt i (Assign vs e)
   Right (v ++ " = " ++ stmt)
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
   Right ("if " ++ expr ++ ":\n\t" ++ tab i ++ stmt)
printStmt i (While e s)           = do
   expr <- printExpr e
   stmt <- printStmt (i+1) s
   Right ("while " ++ expr ++ ":\n\t" ++ tab i ++ stmt)
printStmt i (Output e)            = do
   expr <- printExpr e
   Right ("print(" ++ expr ++ ")")
printStmt i (Block [])            = Right ""
printStmt i (Block [s])           = printStmt i s
printStmt i (Block (s:rest))      = do
   stmt1 <- printStmt i s
   stmt2 <- printStmt i (Block rest)
   Right (stmt1 ++ "\n" ++ tab i ++ stmt2)
printStmt _ (FuncDecl v p ty s)   = do
   expr <- printExpr v
   stmt <- printStmt 1 s
   Right ("def " ++ expr ++ ":\n" ++ tab 1 ++ stmt)
printStmt i (Return e)            =
   case e of
      Nothing -> Right "return"
      Just x  -> do
         expr <- printExpr x
         Right ("return " ++ expr)
printStmt i (OtherS s)            = Left (BadStmt (OtherS s))
printStmt i x                     = error $ show x

printDecl :: Stmt -> String
printDecl (Declare _ [] _)     = ""
printDecl (Declare a (v:vs) e) = " = " ++ v ++ printDecl (Declare a vs e)

printType :: Type -> Either PrettyError String
printType TyInt  = Right "int"
printType TyBool = Right "bool"
printType TyStr  = Right "string"
printType Poly   = Right "poly"
printType (TVar t) = Right ("Type variable: " ++ show t)
printType (TyArr t) = do
   ty <- printType t
   Right ("list[" ++ ty ++ "]")
printType (OtherT t) = Left (BadType (OtherT t))

printExpr :: Expr -> Either PrettyError String
printExpr (Lit l)             = printLit l
printExpr (EVar v)            = Right v
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
   expr <- printExpr v
   arr <- printArray e ""
   Right (expr ++ "(" ++ arr ++ ")")
printExpr (OtherE e)          = Left (BadExpr (OtherE e))

printLit :: Literal -> Either PrettyError String
printLit (LInt n)       = Right (show n)
printLit (LBool True)   = Right "True"
printLit (LBool False)  = Right "False"
printLit (LChar c)      = Right (show c)
printLit (LStr s)       = Right (show s)
printLit (OtherL l)     = Left (BadLit (OtherL l))

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
printString e                      _   = do
   arr <- printArray e ""
   Left (Misc ("Character expected in printString, but the following was found instead: " ++ arr))

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

printUop :: UOp -> Either PrettyError String
printUop Not         = Right "not "
printUop Neg         = Right "-"
printUop (OtherU u)  = Left (BadUop (OtherU u))

