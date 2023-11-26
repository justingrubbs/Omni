

module Omni.Typecheck.Monadic
   (
      checkProg
   ,  Contexts
   )
   where


import            Omni.Data
import qualified  Data.Map          as M
import            Debug.Trace
import            Data.Maybe        (fromMaybe)

import            Control.Monad.State
import            Control.Monad.Reader
import            Control.Monad.Writer
import            Control.Monad.Except
import            Control.Monad.Identity
import            Data.Functor


-- ReaderT Ctx (Either TypeError) Rest
-- ctx <- ask 
-- local (function to modify context) (computation to run locally)

-- Defining Contexts and Monad Transformer Stack:
---------------------------------------------------------------------
-- Ident is a type synonym for strings
type Ctx  = M.Map Ident Type
type SCtx = M.Map Ident [Ident]  -- Shadowed variables
type VCtx = M.Map Ident Ident
type Env  = (Ctx, SCtx, VCtx)

type Count = Int  -- Counter 

type Contexts = ReaderT Env (StateT Count (WriterT [Constraint] (ExceptT TypeError Identity)))

-- runEnv :: Env -> Count -> Prog -> Either TypeError (Prog, [Constraint])
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


-- Variable Generation and Maintenance:
---------------------------------------------------------------------
getExpr :: Stmt -> Contexts Stmt
getExpr (Assign x e)        = do
   e' <- getVars e
   return $ Assign x e'
getExpr (Declare ty x e)    =
   case e of
      Nothing -> return $ Declare ty x e
      Just e' -> do
         expr <- getVars e'
         return $ Declare ty x (Just expr)
getExpr (IfThen e s)        = do
   e' <- getVars e
   s' <- getExpr s
   return $ IfThen e' s'
getExpr (IfElse e s1 s2)    = do
   e' <- getVars e
   s1' <- getExpr s1
   s2' <- getExpr s2
   return $ IfElse e' s1' s2'
getExpr (While e s)         = do
   s' <- getExpr s
   return $ While e s'
getExpr (Block s)           = do
   -- I was unsure how to convert `map (getExpr vCtx) s` to work with monads
      -- ChatGPT offered `sequence $ map getExpr s` which was reduced to `mapM getExpr s`
   s' <- mapM getExpr s
   return $ Block s'
getExpr (ExprStmt e)        = do
   e' <- getVars e
   return $ ExprStmt e'
getExpr (OtherS s)          = return $ OtherS s
getExpr (FuncDecl v e ty s) = return $ FuncDecl v e ty s
getExpr x                   = error $ show x ++ " was not intended to be in prog"

getBlockExpr :: Stmt -> Stmt -> Contexts Stmt
getBlockExpr (Block [])     (Block y) = return $ Block (reverse y)
getBlockExpr (Block (x:xs)) (Block y) = do
   expr <- getExpr x
   getBlockExpr (Block xs) (Block (expr : y))
getBlockExpr _              _         = undefined

eng :: String
eng = ['A'..'Z']  -- Arbitrary letters, is there a more standard way to do this?

base26 :: Int -> String  -- Need to edit this
base26 0 = [head eng]
base26 n = reverse (f n)
  where
   base = length eng
   f 0 = []
   f x =
      let (q, r) = (x - 1) `divMod` base
      in (eng !! r) : f q

getVars :: Expr -> Contexts Expr
getVars (Var x)        = Var <$> getVar x
getVars (Bin op e1 e2) = do
   e1' <- getVars e1
   e2' <- getVars e2
   return $ Bin op e1' e2'
getVars (Un op e1)     = do
   e1' <- getVars e1
   return $ Un op e1'
getVars (Array x)      = do
   x' <- mapM getVars x
   return $ Array x'
getVars (Output e)     = do
   e' <- getVars e
   return $ Output e'
getVars x              = return x

genVar :: Contexts Ident
genVar = do
   i <- get
   ctx <- askC
   let var = base26 i
   case M.lookup var ctx of
      Just x  -> do
         inc
         genVar
      Nothing -> do 
         inc 
         return var

getVar :: Ident -> Contexts Ident
getVar v = Data.Maybe.fromMaybe v . M.lookup v <$> askV


-- Type checking prog:
---------------------------------------------------------------------
checkProg :: Prog -> Either TypeError Prog
checkProg p = do
   (prog, _) <- runEnv (M.empty, M.empty, M.empty) 0 (testProg p)
   -- trace (show prog) $ pure ()
   Right prog

-- checkProg [Assign ["x"] (Lit (Int 3)), Assign ["x"] (Lit (Bool True)), Assign ["x"] (Lit (Int 10)), Assign ["x"] (Lit (Char 'c'))]

testProg :: Prog -> Contexts Prog
testProg p = do
--   env <- ask
  elaborateProg p []

-- I considered making this `elaborateProg :: Stmt -> Contexts Stmt` so that I could use map with it instead of manual recursing through
   -- Decided against it because there would be cases in which I need to split a statement into two statements
elaborateProg :: Prog -> Prog -> Contexts Prog
elaborateProg []                  prog = return prog
elaborateProg (Assign x e : rest) prog = do
   e' <- getVars e
   ty <- infer e'
   let declare = Declare ty [] (Just e')
   let assign  = Assign [] e'
   elabTest (Assign x e' : rest) prog ty declare assign  -- X could be undefined, reassigned, or redefined
elaborateProg (Block s : rest) prog   = do

   s' <- elaborateProg s []
   elaborateProg rest (Block s' : prog)
elaborateProg (IfThen e (Block s) : rest) prog   = do
   e' <- getVars e
   s' <- elaborateProg s []
   elaborateProg rest (IfThen e' (Block s') : prog)
elaborateProg (IfElse e (Block s) o : rest) prog   = do
   e' <- getVars e
   s' <- elaborateProg s []
   s'' <- elaborateProg [o] []
   case s'' of
      [x] -> elaborateProg rest (IfElse e' (Block s') x : prog)
      _   -> error "oops in elaborateProg"
elaborateProg (While e (Block s) : rest) prog = do
   e' <- getVars e
   s' <- elaborateProg s []
   while <- getExpr $ While e' (Block s')
   elaborateProg rest (while : prog)
elaborateProg (stmt : rest) prog      = do
   s' <- getExpr stmt
   elaborateProg rest (s' : prog)

elabTest :: Prog -> Prog -> Type -> Stmt -> Stmt -> Contexts Prog 
elabTest p np ty s1 s2 = do 
   ctx' <- askC 
   sCtx' <- askS 
   vCtx' <- askV 
   trace (show ctx' ++ "\n\n" ++ show sCtx' ++ "\n\n" ++ show vCtx') $ pure ()
   elabAssign p np ty s1 s2

elabAssign :: Prog -> Prog -> Type -> Stmt -> Stmt -> Contexts Prog
elabAssign (Assign [] e : rest) nProg _ (Declare tyD eD e') (Assign eA e'')
   | null eD && null eA = elaborateProg nProg rest
   | null eD            = elaborateProg (Assign (reverse eA) e'' : nProg) rest
   | null eA            = elaborateProg (Declare tyD (reverse eD) e' : nProg) rest
   | otherwise          = elaborateProg (Declare tyD (reverse eD) e' : Assign (reverse eA) e'' : nProg) rest
elabAssign (Assign (x:xs) e : rest) nProg ty da a = do
   vCtx <- askV
   ctx <- askC
   case M.lookup x ctx of
         -- Assignment before declaration is fine in Python, but need to add the declaration for explicit languages
      Nothing -> do
         -- M.insert x ty ctx
         local (\(ctx, sCtx, vCtx) -> (M.insert x ty ctx, sCtx, vCtx)) $ elabTest (Assign xs e : rest) nProg ty (addVar da x) a
      Just y  -> elabAssign' (Assign (x:xs) e : rest) nProg ty y da a
elabAssign other nProg _ _ _ = elaborateProg other nProg

elabAssign' :: Prog -> Prog -> Type -> Type -> Stmt -> Stmt -> Contexts Prog
elabAssign' (Assign (x:xs) e : rest) nProg ty y da a = do 
   vCtx <- askV 
   if x `elem` M.elems vCtx
   then do 
      x' <- genVar
      local (\(ctx, sCtx, vCtx) -> (M.insert x' ty ctx, sCtx, M.insert x x' vCtx)) $ elabTest (Assign xs e : rest) nProg ty (addVar da x') a
   else if y == ty 
   then do
      local (\(ctx, sCtx, vCtx) -> (ctx, sCtx, M.delete x vCtx)) $ elabTest (Assign xs e : rest) nProg ty da (addVar a x)
   else do
      sCtx <- askS
      case M.lookup x sCtx of
         Just vList -> do
            subV <- findSubVariable ty vList
            case subV of
               Just s  -> local (\(ctx, sCtx, vCtx) -> (ctx, sCtx, M.insert x s vCtx)) $ elabTest (Assign xs e : rest) nProg ty da (addVar a s)
               Nothing -> do
                  x' <- genVar
                  local (\(ctx, sCtx, vCtx) -> (M.insert x' ty ctx, M.insert x (x' : vList) sCtx, M.insert x x' vCtx)) $ elabTest (Assign xs e : rest) nProg ty (addVar da x') a
         Nothing    -> do
            x' <- genVar
            local (\(ctx, sCtx, vCtx) -> (M.insert x' ty ctx, M.insert x [x'] sCtx, M.insert x x' vCtx)) $ elabTest (Assign xs e : rest) nProg ty (addVar da x') a
elabAssign' _ _ _ _ _ _ = undefined

-- old-ish version:
-- elabAssign' :: VCtx -> Prog -> Prog -> Type -> Type -> Stmt -> Stmt -> Contexts Prog
-- elabAssign' vCtx (Assign (x:xs) e : rest) nProg ty y da a
--   | x `elem` M.elems vCtx = do
--       x' <- genVar
--       -- M.insert x' ty ctx
--       -- M.insert x x' vCtx 
--       local (\(ctx, sCtx, vCtx) -> (M.insert x' ty ctx, sCtx, M.insert x x' vCtx)) $ elabTest (Assign xs e : rest) nProg ty (addVar da x') a
--   | y == ty               = do
--    -- M.delete x vCtx
--    local (\(ctx, sCtx, vCtx) -> (ctx, sCtx, M.delete x vCtx)) $ elabTest (Assign xs e : rest) nProg ty da (addVar a x)
--   | otherwise             = do
--       sCtx <- askS
--       case M.lookup x sCtx of
--          Just vList -> do
--             subV <- findSubVariable ty vList
--             case subV of
--                Just s  -> do
--                   -- M.insert x s vCtx 
--                   local (\(ctx, sCtx, vCtx) -> (ctx, sCtx, M.insert x s vCtx)) $ elabTest (Assign xs e : rest) nProg ty da (addVar a s)
--                Nothing -> do
--                   x' <- genVar
--                   -- M.insert x' ty ctx 
--                   -- M.insert x (x' : vList) sCtx 
--                   -- M.insert x x' vCtx
--                   local (\(ctx, sCtx, vCtx) -> (M.insert x' ty ctx, M.insert x (x' : vList) sCtx, M.insert x x' vCtx)) $ elabTest (Assign xs e : rest) nProg ty (addVar da x') a
--          Nothing    -> do
--             x' <- genVar
--             -- M.insert x' ty ctx 
--             -- M.insert x [x'] sCtx 
--             -- M.insert x x' vCtx
--             local (\(ctx, sCtx, vCtx) -> (M.insert x' ty ctx, M.insert x [x'] sCtx, M.insert x x' vCtx)) $ elabTest (Assign xs e : rest) nProg ty (addVar da x') a
-- elabAssign' _ _ _ _ _ _ _ = undefined

addVar :: Stmt -> Ident -> Stmt
addVar (Declare ty x (Just e)) var = Declare ty (var : x) (Just e)
addVar (Assign x e)            var = Assign (var : x) e
addVar rest                    _   = error ("Unexpected statement in addVar: " ++ show rest)

findSubVariable :: Type -> [Ident] -> Contexts (Maybe Ident)
findSubVariable ty []     = return Nothing
findSubVariable ty (x:xs) = do
   ctx <- askC
   case M.lookup x ctx of
      Nothing -> error "There will always be something here"
      Just y  ->
         if y == ty
         then return $ Just x
         else findSubVariable ty xs


-- Type inference:
---------------------------------------------------------------------
infer :: Expr -> Contexts Type
infer (Lit (Int n))    = return TyInt
infer (Lit (Bool b))   = return TyBool
infer (Lit (Char c))   = return TyChar
infer (Lit (Str [s]))  = return TyChar
infer (Lit (Str s))    = return TyStr
infer (Var v)          = do
   ctx <- askC
   case M.lookup v ctx of
      Nothing -> throwError $ UndefinedVar v
      Just x  -> return x
infer (Bin op e1 e2)   = do
   e1' <- infer e1
   check e2 e1' *>
      case op of
         And -> check e1 TyBool $> TyBool
         Or  -> check e1 TyBool $> TyBool
         Eq  -> return TyBool
         NEq -> return TyBool
         Less -> return TyBool
         Greater -> return TyBool
         LessEq -> return TyBool
         GreaterEq -> return TyBool
         -- Mul -> Can be num -> num -> num or string -> num -> [string?] regardless, need to allow for two different types      
         Add ->
            if e1' == TyInt
            then return TyInt
            else
               if e1' == TyStr
               then return TyStr
               else do
                  e2' <- infer e2
                  throwError $ TypeMismatch e1' e2'
         x   -> check e1 TyInt $> TyInt
infer (Un op e)        =
   case op of
      Neg -> check e TyInt $> TyInt
      Not -> check e TyBool $> TyBool
      x   -> error $ "The following unary operator is not yet implemented: " ++ show x
infer (Array [])       = return $ TyArr Poly  -- Type constraints needed?
infer (Array (x:rest)) = do
   xTy <- infer x
   inferArray xTy rest
infer x                = error $ "The following expression is not yet implemented: " ++ show x -- so this should show the actual expression

-- inferArray :: Type -> [Expr] -> Contexts Type
-- inferArray ty []       = return $ TyArr ty
-- inferArray ty (x:rest) = do 
--    ctx <- askC 
--    check x ty 
--    *> inferArray ty rest

inferArray :: Type -> [Expr] -> Contexts Type -- Not sure what is going on here, VSCode recommended changes
inferArray ty = foldr
      (\ x
         -> (*>)
              (do ctx <- askC
                  check x ty))
      (return (TyArr ty))

check :: Expr -> Type -> Contexts ()
check e ty = do
   ty' <- infer e
   if ty == ty'
   then return ()
   else throwError $ TypeMismatch ty ty'

