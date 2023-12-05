

module Omni.Typecheck.Data where


import            Omni.Data
import qualified  Data.Map                as M
import            Control.Monad.State
import            Control.Monad.Reader
import            Control.Monad.Writer
import            Control.Monad.Except
import            Control.Monad.Identity
import            Data.Functor            (($>))
import            Data.Bifunctor


-- Defining Contexts and Monad Transformer Stack:
---------------------------------------------------------------------
type Count = Int

-- Function context
type FCtx = M.Map Ident (Stmt, Env)

-- Overloading context 
type OCtx = M.Map Ident [Ident]

type Contexts = ReaderT Env (StateT (Count,FCtx,OCtx) (ExceptT TypeError Identity))

runEnv :: Env 
   -> Count 
   -> Contexts Prog 
   -> Either TypeError Prog
runEnv env i p = runIdentity $ runExceptT $ evalStateT (runReaderT p env) (i,M.empty,M.empty)


-- Helper functions to make accessing data easier:
---------------------------------------------------------------------
askC :: Contexts Ctx
askC = asks (\(ctx, _, _) -> ctx)

askS :: Contexts SCtx
askS = asks (\(_, sCtx, _) -> sCtx)

askV :: Contexts VCtx
askV = asks (\(_, _, vCtx) -> vCtx)

empty :: Env
empty = (M.empty, M.empty, M.empty)


-- Count for variable generation:
---------------------------------------------------------------------
getI :: Contexts Count
getI = gets (\(i,_,_) -> i)

putI :: Count -> Contexts ()
putI i = modify (\(_,fCtx,oCtx) -> (i,fCtx,oCtx))

inc :: Int -> Contexts ()
inc i = putI (i + 1)


-- FCtx for inference on functions:
---------------------------------------------------------------------
getF :: Contexts FCtx
getF = gets (\(_,fCtx,_) -> fCtx)

getStmt :: Ident -> Contexts Stmt
getStmt ident = do
   fCtx <- getF
   env <- ask
   case M.lookup ident fCtx of
      Nothing       -> throwError $ UndefinedFun ident env
      Just (stmt,_) -> return stmt

putStmt :: Ident -> Stmt -> Contexts ()
putStmt ident stmt = do
   env <- getEnv ident
   modify (\(i,fCtx,oCtx) -> (i,M.insert ident (stmt,env) fCtx,oCtx))

getEnv :: Ident -> Contexts Env
getEnv ident = do
   fCtx <- getF
   case M.lookup ident fCtx of
      Nothing      -> return empty
      Just (_,env) -> return env

putEnv :: Ident -> Env -> Contexts ()
putEnv ident env = do
   stmt <- getStmt ident
   modify (\(i,fCtx,oCtx) -> (i,M.insert ident (stmt,env) fCtx,oCtx))

getArgs :: Ident -> Contexts [Args]
getArgs ident = do 
   fCtx <- getF 
   case M.lookup ident fCtx of 
      Nothing                   -> throwError $ UndefinedFun ident empty
      Just (FunDecl _ a _ _,_)  -> return a
      Just _                    -> error "Weirdness in getArgs"


-- OCtx for overloading:
---------------------------------------------------------------------
getO :: Contexts OCtx
getO = gets (\(_,_,oCtx) -> oCtx)

putO :: Ident -> Stmt -> Env -> Contexts ()
putO ident stmt env = do 
   ident' <- genFunc 
   oCtx <- getO 
   case M.lookup ident oCtx of 
      Nothing -> modify (\(i,fCtx,oCtx) 
         -> (i,M.insert ident' (stmt,env) fCtx,M.insert ident [ident'] oCtx))
      Just x  -> modify (\(i,fCtx,oCtx) 
         -> (i,M.insert ident' (stmt,env) fCtx,M.insert ident (ident' : x) oCtx))

genFunc :: Contexts Ident 
genFunc = do 
   i <- getI 
   ident <- genFunc' 
   putI i 
   return ident

genFunc' :: Contexts Ident
genFunc' = do
   ident <- base26
   fCtx <- getF 
   oCtx <- getO
   case M.lookup ident fCtx of
      Just _  -> genFunc'
      Nothing -> 
         case M.lookup ident oCtx of 
            Just _ -> genFunc'
            Nothing -> return ident

eng :: String 
eng = ['A'..'Z']

base26 :: Contexts String
base26 = do
   i <- getI
   inc i
   case i of
      0 -> return [head eng]
      n -> return $ reverse (f n)
         where
            base = length eng
            f 0 = []
            f x =
               if i < 26
               then let (q, r) = x `divMod` base
                  in (eng !! r) : f q
               else let (q, r) = (x - 1) `divMod` base
                  in (eng !! r) : f q

