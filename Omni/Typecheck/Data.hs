

module Omni.Typecheck.Data where 


import            Omni.Data 
import qualified  Data.Map                as M
import            Control.Monad.State
import            Control.Monad.Reader
import            Control.Monad.Writer
import            Control.Monad.Except
import            Control.Monad.Identity


-- Defining Contexts and Monad Transformer Stack:
---------------------------------------------------------------------

-- Context that maps variables to their types
type Ctx  = M.Map Ident Type

-- Context that maps variables to an array containing their shadowed variables
type SCtx = M.Map Ident [Ident]

-- Context that maps source variables to the current generated variable (or Nothing if the source variable is current)
type VCtx = M.Map Ident Ident

type Env  = (Ctx, SCtx, VCtx)

type Count = Int

type Contexts = ReaderT Env (StateT Count (WriterT [Constraint] (ExceptT TypeError Identity)))

runEnv :: Monoid w =>
   r
   -> s
   -> ReaderT r (StateT s (WriterT w (ExceptT e Identity))) a
   -> Either e (a, w)
runEnv env i p = runIdentity $ runExceptT $ runWriterT $ evalStateT (runReaderT p env) i

askC :: Contexts Ctx
askC = asks (\(ctx, _, _) -> ctx)

askS :: Contexts SCtx
askS = asks (\(_, sCtx, _) -> sCtx)

askV :: Contexts VCtx
askV = asks (\(_, _, vCtx) -> vCtx)

inc :: (Num i, MonadState i m) => m ()
inc = do
   i <- get
   put (i + 1)

empty :: Env 
empty = (M.empty, M.empty, M.empty)

