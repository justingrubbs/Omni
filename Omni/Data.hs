{-# LANGUAGE GADTs #-}

module Omni.Data where


type Prog = [Stmt]
type Ident = String


data Lang where 
   Python :: Lang 
   Java   :: Lang 
   deriving Show

data Type where 
   Poly     :: Type
   TyInt    :: Type
   TyFloat  :: Type
   TyDouble :: Type
   TyBool   :: Type
   TyChar   :: Type
   TyStr    :: Type
   TyVoid   :: Type
   TyArr    :: Type -> Type
   TVar     :: Ident -> Type
   OtherT   :: String -> Type
   deriving (Show, Eq)

-- After talking shit about the way the Java parser works, I would like to change mine to be more like it
   -- Specifically, make a new type Assign that has two? one? constructor so that elabAssign does not have to have a wildcard

-- apparently for loops are just while loops, makes sense but didn't know
data Stmt where 
   Assign    :: [Ident] -> Expr -> Stmt 
   AugAssign :: Ident -> AOp -> Expr -> Stmt
   Declare   :: Type -> [Ident] -> Maybe Expr -> Stmt
   Block     :: Prog -> Stmt
   IfThen    :: Expr -> Stmt -> Stmt 
   IfElse    :: Expr -> Stmt -> Stmt -> Stmt 
   While     :: Expr -> Stmt -> Stmt 
-- For       :: Stmt
   FuncDecl  :: Ident -> [Args] -> Type -> Stmt -> Stmt
   ExprStmt  :: Expr -> Stmt
   Return    :: Maybe Expr -> Stmt
   OtherS    :: String -> Stmt
   deriving Show 

data Args where 
   Args :: Type -> Ident -> Args 
   deriving Show

data Expr where 
   Lit    :: Literal -> Expr
   Var    :: Ident -> Expr 
   Array  :: [Expr] -> Expr 
   ArrayA :: [Expr] -> Type -> Expr -- for re-assigning arrays in Java
   Bin    :: BOp -> Expr -> Expr -> Expr
   Un     :: UOp -> Expr -> Expr
-- PredefinedCall :: idk      -- append() in python and others (Print also probably)
   Call   :: [Ident] -> [Expr] -> Expr
   Output :: Expr -> Expr 
   OtherE :: String -> Expr
   deriving Show

data Literal where 
   Int    :: Integer -> Literal 
   Float  :: Float -> Literal 
   Double :: Double -> Literal
   Bool   :: Bool -> Literal 
   Char   :: Char -> Literal 
   Str    :: String -> Literal 
   OtherL :: String -> Literal
   deriving Show

-- https://www.w3schools.com/python/python_operators.asp
-- https://www.w3schools.com/java/java_operators.asp

-- Java does not have exponentiation built in?
-- Later, could maybe use Math.pow() or whatever add the import in if exp is used 

data BOp = Add | Sub | Mul | Div | Mod | Exp
   | And | Or | Eq | NEq | Less | Greater | LessEq | GreaterEq
   | BWAnd | BWOr | XOR | BWNot | LShift | RShift  -- Bitwise operations
   | OtherB String
   deriving (Show, Eq)

data UOp = Not | Neg | Incr | Decr | OtherU String
   deriving (Show, Eq)

data AOp = AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
   | ExpAssign | BinAndAssign | BinOrAssign | BinXorAssign
   | LeftShiftAssign | RightShiftAssign | FloorDivAssign | MatrixMulAssign 
   | OtherA String
   deriving (Show, Eq)

data Error where 
   ParseError :: ParseError -> Error
   TypeError :: TypeError -> Error 
   ConvertError :: ConvertError -> Error 
   PrettyError :: PrettyError -> Error 
   deriving Show 

data ParseError where 
   Generic :: String -> ParseError 
   deriving Show

data TypeError where 
   TypeMismatch :: Type -> Type -> TypeError
   UndefinedVar :: Ident -> TypeError
   DuplicateVar :: Ident -> TypeError
   deriving Show

data ConvertError where 
   BadConvert :: String -> ConvertError 
   deriving Show

data PrettyError where 
   BadStmt :: Stmt -> PrettyError 
   BadExpr :: Expr -> PrettyError 
   BadType :: Type -> PrettyError 
   BadLit  :: Literal -> PrettyError 
   BadBop  :: BOp -> PrettyError 
   BadUop  :: UOp -> PrettyError 
   BadAop  :: AOp -> PrettyError 
   Misc    :: String -> PrettyError
   deriving Show

data Constraint where 
   Stuff :: Constraint 
   deriving Show

