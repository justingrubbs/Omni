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
   TyArr  :: Type -> Type
   TVar   :: Var -> Type
   OtherT :: String -> Type
   deriving (Show, Eq)

-- apparently for loops are just while loops, makes sense but didn't know
data Stmt where 
   Assign  :: [Var] -> Expr -> Stmt 
   Declare :: Type -> [Var] -> Maybe Expr -> Stmt
   Output  :: Expr -> Stmt
   Block   :: Prog -> Stmt
   IfThen  :: Expr -> Stmt -> Stmt 
   IfElse  :: Expr -> Stmt -> Stmt -> Stmt 
   OtherS  :: String -> Stmt
   deriving Show 

data Expr where 
   Lit    :: Literal -> Expr
   EVar   :: Var -> Expr 
   Array  :: [Expr] -> Expr 
   Bin    :: BOp -> Expr -> Expr -> Expr
   Un     :: UOp -> Expr -> Expr
   OtherE :: String -> Expr
   deriving Show

data Literal where 
   LInt   :: Integer -> Literal 
   LBool  :: Bool -> Literal 
   LChar  :: Char -> Literal 
   LStr   :: String -> Literal 
   OtherL :: String -> Literal
   deriving Show

-- https://www.w3schools.com/python/python_operators.asp
-- https://www.w3schools.com/java/java_operators.asp

-- Java does not have exponentiation built in?
-- Later, could maybe use Math.pow() or whatever add the import in if exp is used 

data BOp = Add | Sub | Mul | Div | Mod
   | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
   | And | Or | Eq | NEq | Less | Greater | LessEq | GreaterEq
   | BWAnd | BWOr | XOR | BWNot | LShift | RShift  -- Bitwise operations
   | OtherB String
   deriving (Show, Eq)

data UOp = Not | Neg | Incr | Decr | OtherU String
   deriving (Show, Eq)

data TypeError where 
   TypeMismatch :: Type -> Type -> TypeError
   UndefinedVar :: Var -> TypeError
   DuplicateVar :: Var -> TypeError
   deriving Show

