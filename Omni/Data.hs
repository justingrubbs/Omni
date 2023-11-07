{-# LANGUAGE GADTs #-}

module Omni.Data where


type Prog = [Stmt]
type Var  = String


data Lang where 
   Python :: Lang 
   Java   :: Lang 
   deriving Show

data Type where 
   Poly   :: Type
   TyInt  :: Type
   TyBool :: Type
   TyChar :: Type
   TyStr  :: Type
   TVar   :: Var -> Type
   deriving (Show, Eq)

data Stmt where 
   Assign  :: Var -> Expr -> Stmt 
   Declare :: Type -> Var -> Stmt
   DAndA   :: Type -> Var -> Expr -> Stmt
   Output  :: Expr -> Stmt
   Block   :: Prog -> Stmt
   OtherS  :: Stmt
   deriving Show 

data Expr where 
   EInt  :: Integer -> Expr 
   EBool :: Bool -> Expr
   EChar :: Char -> Expr
   EStr  :: String -> Expr
   EVar  :: Var -> Expr 
   Bin   :: BOp -> Expr -> Expr -> Expr
   Un    :: UOp -> Expr -> Expr
   OtherE :: Expr
   deriving Show

-- https://www.w3schools.com/python/python_operators.asp
-- https://www.w3schools.com/java/java_operators.asp

-- Java does not have exponentiation built in?
-- Later, could maybe use Math.pow() or whatever add the import in if exp is used 
data BOp = Add | Sub | Mul | Div | Mod
   | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign | ExpAssign
   | And | Or | Eq | NEq | Less | Greater | LessEq | GreaterEq
   | BWAnd | BWOr | XOR | BWNot | LShift | RShift  --Bitwise operations
   deriving (Show, Eq)

data UOp = Not | Neg | Incr | Decr
   deriving (Show, Eq)

data TypeError where 
   TypeMismatch :: Type -> Type -> TypeError
   UndefinedVar :: Var -> TypeError
   DuplicateVar :: Var -> TypeError
   deriving Show

