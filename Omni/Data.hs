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

-- apparently for loops are just while loops, makes sense but didn't know
data Stmt where 
   Assign    :: [Ident] -> Expr -> Stmt 
   AugAssign :: Ident -> AOp -> Expr -> Stmt
   Declare   :: Type -> [Ident] -> Maybe Expr -> Stmt
   -- Output    :: Expr -> Stmt
   Block     :: Prog -> Stmt
   IfThen    :: Expr -> Stmt -> Stmt 
   IfElse    :: Expr -> Stmt -> Stmt -> Stmt 
   While     :: Expr -> Stmt -> Stmt 
   -- For       :: Stmt
   FuncDecl  :: Ident -> [Expr] -> Type -> Stmt -> Stmt
   ExprStmt  :: Expr -> Stmt
   Return    :: Maybe Expr -> Stmt
   OtherS    :: String -> Stmt
   deriving Show 

data Expr where 
   Lit    :: Literal -> Expr
   Var    :: Ident -> Expr 
   Args   :: Type -> Ident -> Expr
   Array  :: [Expr] -> Expr 
   Bin    :: BOp -> Expr -> Expr -> Expr
   Un     :: UOp -> Expr -> Expr
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

data TypeError where 
   TypeMismatch :: Type -> Type -> TypeError
   UndefinedVar :: Ident -> TypeError
   DuplicateVar :: Ident -> TypeError
   deriving Show

data ConversionError where 
   BadConvert :: String -> ConversionError 
   deriving Show

