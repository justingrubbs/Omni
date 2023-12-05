{-# LANGUAGE GADTs #-}

module Omni.Data where


import qualified Data.Map as M


type Prog = [Stmt]
type Ident = String

-- Context that maps variables to their types
type Ctx  = M.Map Ident Type

-- Context that maps variables to an array containing their shadowed variables
type SCtx = M.Map Ident [Ident]

-- Context that maps source variables to the current generated variable (or Nothing if the source variable is current)
type VCtx = M.Map Ident Ident

type Env  = (Ctx, SCtx, VCtx)


data Lang where 
   Python :: Lang 
   Java   :: Lang 
   deriving Show


data Stmt where 
   Assign    :: [Ident] -> Expr -> Stmt 
   AugAssign :: Ident -> AOp -> Expr -> Stmt
   Declare   :: Type -> [Ident] -> Maybe Expr -> Stmt
   Block     :: Prog -> Stmt
   IfThen    :: Expr -> Stmt -> Stmt 
   IfElse    :: Expr -> Stmt -> Stmt -> Stmt 
   While     :: Expr -> Stmt -> Stmt 
-- For       :: Stmt
   FunDecl   :: Ident -> [Args] -> Type -> Stmt -> Stmt
   ExprStmt  :: Expr -> Stmt
   Return    :: Maybe Expr -> Stmt
   OtherS    :: String -> Stmt
   deriving (Show,Eq) 

data Args where 
   Args :: Type -> Ident -> Args 
   deriving (Show,Eq)

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
   deriving (Show,Eq)

data Literal where 
   Int    :: Integer -> Literal 
   Float  :: Float -> Literal 
   Double :: Double -> Literal
   Bool   :: Bool -> Literal 
   Char   :: Char -> Literal 
   Str    :: String -> Literal 
   Null   :: Literal
   OtherL :: String -> Literal
   deriving (Show, Eq)


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
   TyArgs   :: [Type] -> Type
   TVar     :: Ident -> Type
   OtherT   :: String -> Type
   deriving (Show, Eq)

data SType where 
   Num  :: SType 
   Text :: SType 
   None :: SType 
   deriving Show


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
   TypeMismatch  :: Type -> Type -> Env -> TypeError
   STypeMismatch :: Type -> SType -> Env -> TypeError
   UndefinedVar  :: Ident -> Env -> TypeError
   DuplicateVar  :: Ident -> Env -> TypeError
   BadVoid       :: Expr -> Env -> TypeError
   VoidArray     :: String -> Env -> TypeError
   UndefinedFun  :: Ident -> Env -> TypeError
   MainType      :: Ident -> Type -> Env -> TypeError
   MainArgs      :: Ident -> [Args] -> Env -> TypeError
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

