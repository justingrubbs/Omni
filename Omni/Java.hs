module Omni.Java 
   (
      javaParse, checkJavaParse, printJava
   )
   where 


import            Omni.Data 
import qualified  Language.Java.Parser    as P
import qualified  Language.Java.Syntax    as S


-- Absurd
{- CompilationUnit Nothing [] [ClassTypeDecl (ClassDecl [public] (Ident "pyTest") [] Nothing [] 
   (ClassBody [MemberDecl (MethodDecl [public,static] [] Nothing (Ident "main") 
   [FormalParam [] (RefType (ArrayType (RefType (ClassRefType (ClassType [(Ident "String",[])]))))) 
   False (VarId (Ident "args"))] [] Nothing (MethodBody (Just (Block [LocalVars [] (PrimType IntT) 
   [VarDecl (VarId (Ident "x")) (Just (InitExp (Lit (Int 3))))],LocalVars [] (PrimType BooleanT) 
   [VarDecl (VarId (Ident "y")) (Just (InitExp (Lit (Boolean True))))]]))))]))]  -}


javaParse :: String -> Either String Prog 
javaParse txt = 
   case P.parser P.compilationUnit txt of 
      Left  err -> error (show err)
      Right x   -> Right (convertJavaAST x) 

convertExpr :: S.Exp -> Expr 
convertExpr (S.Lit x) = 
   case x of 
      S.Int y     -> EInt y 
      S.Boolean y -> EBool y
      _           -> OtherE 
convertExpr (S.ExpName (S.Name (S.Ident name:_))) = EVar name

convertStmt :: S.Stmt -> Stmt
convertStmt (S.ExpStmt (S.Assign (S.NameLhs (S.Name (S.Ident name:_))) _ x)) = Assign name (convertExpr x)

convertVar :: S.VarDecl -> Type -> Stmt 
convertVar (S.VarDecl (S.VarId (S.Ident name)) maybe) t = 
   case maybe of 
      Nothing -> Declare t name
      Just x  -> 
         case x of 
            S.InitExp y -> DAndA t name (convertExpr y)
            _           -> OtherS
convertVar x _ = error ("Bad pattern match in convertVar =(\nThe problem: " ++ show x) -- replace iwth other prob?

convertBlock :: S.BlockStmt -> Stmt
convertBlock (S.BlockStmt x)     = convertStmt x 
convertBlock (S.LocalVars _ (S.PrimType t) (x:_)) = convertVar x (convertType t)

convertType :: S.PrimType -> Type 
convertType S.IntT = TyInt
convertType S.BooleanT = TyBool 
convertType _ = undefined

convertJavaAST :: S.CompilationUnit -> Prog 
convertJavaAST (S.CompilationUnit _ _ x) = 
   case x of 
      [S.ClassTypeDecl (S.ClassDecl _ _ _ _ _ (S.ClassBody (S.MemberDecl (S.MethodDecl _ _ _ _ _ _ _ (S.MethodBody y)) : rest)))] -> 
         case y of 
            Nothing           -> []
            Just (S.Block z)  -> map convertBlock z
      _ -> undefined

checkJavaParse :: FilePath -> IO ()
checkJavaParse file = do
   s <- readFile file
   case javaParse s of
      Left err -> print err
      Right p  -> print p


-- Pretty-Printing:
---------------------------------------------------------------------

printJava :: String -> Prog -> String 
printJava name prog = "public class " ++ name ++ " {\n\tpublic static void main(String[] args) {\n" ++ prettyJava "" prog ++ "\n\t}\n}"

prettyJava :: String -> Prog -> String
prettyJava = foldl (\ text x -> text ++ "\t\t" ++ printStmt x ++ ";\n")

printStmt :: Stmt -> String
printStmt (Assign var e)     = var ++ " = " ++ printExpr e
printStmt (Declare ty var)   = printType ty ++ " " ++ var
printStmt (DAndA ty var e)   = printType ty ++ " " ++ var ++ " = " ++ printExpr e
printStmt (Output e)         = "System.out.println(" ++ printExpr e ++ ")"

printType :: Type -> String
printType TyInt    = "int"
printType TyBool   = "boolean"
printType TyChar   = "char"
printType Poly     = "<Type unknown>"
printType (TVar t) = "Type variable: " ++ show t 

printExpr :: Expr -> String
printExpr (EInt n)      = show n
printExpr (EBool True)  = "true"
printExpr (EBool False) = "false"
printExpr (EChar c)     = show c
printExpr (EVar v)      = v
printExpr (Bin bop x y) = printExpr x ++ printBop bop ++ printExpr y
printExpr (Un uop x)    = printUop uop ++ printExpr x

printBop :: BOp -> String 
printBop Add = " + "
printBop Sub = " - "
printBop Mul = " * "
printBop Div = " / "
printBop Mod = " % "
printBop And = " && "
printBop Or  = " || "
printBop Eq  = " == "
printBop NEq = " != "
printBop Less = " < "
printBop Greater = " > "
printBop LessEq = " <= "
printBop GreaterEq = " >= "

printUop :: UOp -> String 
printUop Not = "!"
printUop Neg = "-"

