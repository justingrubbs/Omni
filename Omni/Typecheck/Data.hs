

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

type Contexts = ReaderT Env (StateT (Count,FCtx) (ExceptT TypeError Identity))

runEnv :: Env 
   -> Count 
   -> Contexts Prog 
   -> Either TypeError Prog
-- runEnv :: r
--    -> a1
--    -> ReaderT r (StateT (a1, M.Map k a2) (ExceptT e Identity)) a3
--    -> Either e a3
runEnv env i p = runIdentity $ runExceptT $ evalStateT (runReaderT p env) (i,M.empty)

add :: Num a => a -> a -> a
add = (+)

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
getI = gets fst

putI :: Count -> Contexts ()
putI i = modify (\(_,fCtx) -> (i,fCtx))

inc :: Int -> Contexts ()
inc i = putI (i + 1)


-- FCtx for inference on functions:
---------------------------------------------------------------------
getF :: Contexts FCtx
getF = gets snd

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
   modify (second (M.insert ident (stmt, env)))

getArgs :: Ident -> Contexts [Args]
getArgs ident = do 
   fCtx <- getF 
   case M.lookup ident fCtx of 
      Nothing                   -> throwError $ UndefinedFun ident empty
      Just (FunDecl _ a _ _,_) -> return a
      Just _                    -> error "Weirdness in getArgs"

getEnv :: Ident -> Contexts Env
getEnv ident = do
   fCtx <- getF
   case M.lookup ident fCtx of
      Nothing      -> return empty
      Just (_,env) -> return env

putEnv :: Ident -> Env -> Contexts ()
putEnv ident env = do
   stmt <- getStmt ident
   modify (second (M.insert ident (stmt, env)))

