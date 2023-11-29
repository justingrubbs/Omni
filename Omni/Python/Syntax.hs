

module Omni.Python.Syntax
   (
      pyParse, checkPyParse
   )
   where


import            Omni.Data
import qualified  Language.Python.Version3.Parser     as P
import qualified  Language.Python.Common              as PC
import qualified  Language.Python.Common.Pretty       as T


-- Parsing Python Module:
---------------------------------------------------------------------
pyParse :: String -> String -> Either Error Prog
pyParse txt f =
   case P.parseModule txt f of
      Left  err   -> Left $ ParseError $ Generic $ show err
      Right (p,_) -> Right (convertPythonAST p)
      -- Right (p,_) -> error $ show p

checkPyParse :: FilePath -> IO ()
checkPyParse file = do
   s <- readFile file
   case pyParse s file of
      Left err -> print err
      Right p  -> print p


-- Converting to Omni AST:
---------------------------------------------------------------------
convertPythonAST :: PC.ModuleSpan -> Prog
convertPythonAST (PC.Module prog) = map convertStmt prog


-- Statements:
---------------------------------------------------------------------
convertStmt :: PC.StatementSpan -> Stmt
convertStmt (PC.StmtExpr e _) 
   = ExprStmt (convertExpr e)
convertStmt (PC.Assign a e _) 
   = Assign (convertEIdent a []) (convertExpr e)
convertStmt (PC.AnnotatedAssign _ (PC.Var v _) (Just e) _) 
   = Assign [convertIdent v] (convertExpr e)
convertStmt (PC.AugmentedAssign (PC.Var v _) op e _) 
   = AugAssign (convertIdent v) (convertAOp op) (convertExpr e)
convertStmt (PC.Conditional [(e,s)] [] _) 
   = IfThen (convertExpr e) (Block (map convertStmt s))
convertStmt (PC.Conditional [(e,s)] x y) 
   = IfElse (convertExpr e) (Block (map convertStmt s)) (Block (map convertStmt x))
convertStmt (PC.Conditional ((e,s):sz) x y) 
   = IfElse (convertExpr e) (Block (map convertStmt s)) (convertStmt (PC.Conditional sz x y))
convertStmt (PC.While e s [] _) 
   = While  (convertExpr e) (Block (map convertStmt s))
convertStmt (PC.Fun v para _ s _) 
   = FuncDecl (convertIdent v) (map convertParam para) Poly (Block (map convertStmt s))
convertStmt (PC.Return e _) 
   = case e of
      Nothing  -> Return Nothing
      Just exp -> Return (Just (convertExpr exp))
convertStmt rest 
   = OtherS (show rest)

convertParam :: PC.ParameterSpan -> Args
convertParam (PC.Param v _ _ _)  = Args Poly (convertIdent v) -- Poly just a placeholder for now 
convertParam rest                = error ("Unmatched pattern in convertParam: " ++ show rest)


-- Expressions:
---------------------------------------------------------------------
   -- I have absolutely no idea what the `p` in Int and Float are supposed to be for
convertExpr :: PC.ExprSpan -> Expr
convertExpr (PC.Var v _)             = Var (convertIdent v)
convertExpr (PC.Int i p _)           = Lit (Int i)
convertExpr (PC.Float f p _)         = Lit (Float (realToFrac f))
convertExpr (PC.Bool b _)            = Lit (Bool b)
convertExpr (PC.Strings x _)         = Lit (Str (init $ tail $ concat x))
convertExpr (PC.BinaryOp op e1 e2 _) = Bin (convertBOp op) (convertExpr e1) (convertExpr e2)
convertExpr (PC.UnaryOp op e _)      = Un (convertUOp op) (convertExpr e)
convertExpr (PC.List e _)            = Array (map convertExpr e)
convertExpr (PC.Paren e _)           = convertExpr e
convertExpr (PC.Call e a _)          = 
   case convertExpr e of 
      Var "print" -> 
         let (a':_) = a 
         in Output (convertArg a')
      _           -> Call (convertEIdent [e] []) (map convertArg a)
convertExpr rest                     = OtherE (T.prettyText rest)


-- Identifiers and arguments:
---------------------------------------------------------------------
convertArg :: PC.ArgumentSpan -> Expr
convertArg (PC.ArgExpr e _) = convertExpr e
convertArg rest             = OtherE (T.prettyText rest)

convertIdent :: PC.IdentSpan -> String
convertIdent (PC.Ident s _) = s

convertEIdent :: [PC.ExprSpan] -> [Ident] -> [Ident]
convertEIdent []                                    vars = reverse vars
convertEIdent (PC.Var v _ : rest)                   vars = 
   convertEIdent rest (convertIdent v : vars)
convertEIdent (PC.Tuple [] _ : rest)                vars = reverse vars
convertEIdent (PC.Tuple (PC.Var x _ : vs) _ : rest) vars = 
   convertEIdent vs (convertIdent x : vars)
convertEIdent x                                     _    = error $ show x


-- Operators:
---------------------------------------------------------------------
convertBOp :: PC.OpSpan -> BOp
convertBOp (PC.Plus _)              = Add
convertBOp (PC.Minus _)             = Sub
convertBOp (PC.Multiply _)          = Mul
convertBOp (PC.Divide _)            = Div
-- convertBOp (PC.FloorDivide _) = undefined 
convertBOp (PC.Exponent _)          = Exp
convertBOp (PC.Modulo _)            = Mod
convertBOp (PC.And _)               = And
convertBOp (PC.Or _)                = Or
convertBOp (PC.LessThan _)          = Less
convertBOp (PC.GreaterThan _)       = Greater
convertBOp (PC.LessThanEquals _)    = LessEq
convertBOp (PC.GreaterThanEquals _) = GreaterEq
convertBOp (PC.Equality _)          = Eq
convertBOp (PC.NotEquals _)         = NEq
convertBOp rest                     = OtherB (T.prettyText rest)

convertUOp :: PC.OpSpan -> UOp
convertUOp (PC.Not _) = Not
convertUOp rest       = OtherU (T.prettyText rest)

convertAOp :: PC.AssignOpSpan -> AOp
convertAOp (PC.PlusAssign _)  = AddAssign
convertAOp (PC.MinusAssign _) = SubAssign
convertAOp rest               = OtherA (T.prettyText rest)

