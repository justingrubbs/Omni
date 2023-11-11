{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Omni.Java 
   (
      javaParse, checkJavaParse, printJava
   )
   where 


import            Omni.Data 
import qualified  Language.Java.Parser    as P
import qualified  Language.Java.Syntax    as S
import            Language.Java.Pretty


javaParse :: String -> Either String Prog 
javaParse txt = 
   case P.parser P.compilationUnit txt of 
      Left  err -> error (show err)
      -- Right x   -> Right (convertJavaAST x)
      -- Temporary: =)
      Right x   ->  
         let prog = convertJavaAST x 
         in Right (reverse (modifyProg [] prog))

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
   -- This is weird because it's an array but I think I want it to just be a bunch of individual statements?
   -- idk though will have to test a bit :):)))=+))=)):( block ?? ? ?

convertDecl :: S.Decl -> Stmt 
convertDecl (S.MemberDecl x) = convertMemberDecl x
convertDecl initDecl         = OtherS (prettyPrint initDecl)

convertMemberDecl :: S.MemberDecl -> Stmt -- once implementing functions, change this
convertMemberDecl (S.MethodDecl _ _ _ _ _ _ _ x) = convertMethodBody x
convertMemberDecl rest                           = OtherS (prettyPrint rest)

convertMethodBody :: S.MethodBody -> Stmt
convertMethodBody (S.MethodBody (Just x)) = convertBlock x
convertMethodBody (S.MethodBody Nothing)  = Block []

convertBlock :: S.Block -> Stmt 
convertBlock (S.Block x) = Block (map convertBlockStmt x)

convertBlockStmt :: S.BlockStmt -> Stmt
convertBlockStmt (S.BlockStmt stmt)      = convertStmt stmt
   -- Will eventually need to figure out how to deal with the wildcard !!!
convertBlockStmt (S.LocalVars _ t (x:_)) = convertVarDecl x (convertType t)
convertBlockStmt localClass              = OtherS (prettyPrint localClass)

convertStmt :: S.Stmt -> Stmt 
convertStmt (S.StmtBlock b)                       = convertBlock b
convertStmt (S.ExpStmt (S.Assign lhs S.EqualA e)) = 
   case lhs of 
      S.NameLhs (S.Name (x:_)) -> Assign (convertIdent x) (convertExpr e)
      x                        -> OtherS (prettyPrint lhs)
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
convertRefType (S.ArrayType t) = TyArr (convertType t)
convertRefType classRefType    = OtherT (prettyPrint classRefType)

convertVarDecl :: S.VarDecl -> Type -> Stmt 
convertVarDecl (S.VarDecl (S.VarId x) y) ty = 
   case y of 
      Nothing -> Declare ty (convertIdent x) Nothing 
      Just z  -> Declare ty (convertIdent x) (Just (convertVarInit z))
convertVarDecl (S.VarDecl (S.VarDeclArray x) y) ty = 
   let x' = checkVarDecl x 
   in case y of 
         Nothing -> Declare ty x' Nothing
         Just z  -> Declare ty x' (Just (convertVarInit z))
   
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
convertExpr (S.ExpName (S.Name (x:rest))) = EVar (convertIdent x) 
convertExpr (S.BinOp e1 op e2)            = Bin (convertBinOp op) (convertExpr e1) (convertExpr e2)
-- convertExpr (unary shit) 
convertExpr rest                          = OtherE (prettyPrint rest)

convertLit :: S.Literal -> Literal 
convertLit (S.Int i)     = LInt i 
convertLit (S.Boolean b) = LBool b
convertLit (S.Char c)    = LChar c
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


-- Pretty-Printing:
---------------------------------------------------------------------

printJava :: String -> Prog -> String 
printJava name prog = "public class " ++ name ++ " {\n\tpublic static void main(String[] args) {\n" ++ prettyJava "" prog ++ "\n\t}\n}"

prettyJava :: String -> Prog -> String
prettyJava = foldl (\text x -> text ++ "\t\t" ++ printStmt x ++ ";\n")

printStmt :: Stmt -> String
printStmt (Assign var e)     = var ++ " = " ++ printExpr e
printStmt (Declare ty var e) = 
   case e of 
      Nothing -> printType ty ++ " " ++ var
      Just e' -> printType ty ++ " " ++ var ++ " = " ++ printExpr e'
printStmt (Output e)         = "System.out.println(" ++ printExpr e ++ ")"
printStmt (OtherS _) = "Statement not yet implemented =("

printType :: Type -> String
printType TyInt    = "int"
printType TyBool   = "boolean"
printType TyChar   = "char"
printType TyStr    = "String"
printType (TyArr x) = printType x ++ "[]"
printType Poly     = "<Type unknown>"
printType (TVar t) = "Type variable: " ++ show t 
printType (OtherT _) = "Type not yet implemented =("
printType other    = show other

printExpr :: Expr -> String
printExpr (Lit (LInt n))      = show n
printExpr (Lit (LBool True))  = "true"
printExpr (Lit (LBool False)) = "false"
printExpr (Lit (LChar c))     = show c
printExpr (Lit (LStr s))      = show s
printExpr (EVar v)      = v
printExpr (Array arr)   = "{" ++ reverse (printArray arr "") ++ "}"
printExpr (Bin bop x y) = printExpr x ++ printBop bop ++ printExpr y
printExpr (Un uop x)    = printUop uop ++ printExpr x
printExpr (OtherE _) = "Expression not yet implemented =("

printArray :: [Expr] -> String -> String 
printArray []       str = ""
printArray [x]      str = reverse (printExpr x) ++ str
printArray (x:rest) str = printArray rest ("," ++ reverse (printExpr x) ++ str)

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
printBop (OtherB _) = "Binary operator not yet implemented =("

printUop :: UOp -> String 
printUop Not = "!"
printUop Neg = "-"
printUop (OtherU _) = "Unary operator not yet implemented =("

