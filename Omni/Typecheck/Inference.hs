

module Omni.Typecheck.Inference
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
type VCtx = M.Map Ident Ident    -- Cannot think of a good name but reflects current variable (either original or generated variable)
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


-- Variable Generation and Maintenance:
---------------------------------------------------------------------
getExpr :: Stmt -> Contexts Stmt
getExpr (Assign x e)        = do
   e' <- getVars e
   return $ Assign x e'
getExpr (AugAssign v a e)   = do 
   e' <- getVars e 
   return $ AugAssign v a e'
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
      -- ChatGPT offered `sequence $ map getExpr s` which VSCode recommended reducing to `mapM getExpr s`
   s' <- mapM getExpr s
   return $ Block s'
getExpr (ExprStmt e)        = do
   e' <- getVars e
   return $ ExprStmt e'
getExpr (FuncDecl v e ty s) = return $ FuncDecl v e ty s
getExpr (Return e)          = return $ Return e
getExpr (OtherS s)          = return $ OtherS s
-- getExpr x                   = error $ show x ++ " was not intended to be in prog"

getBlockExpr :: Stmt -> Stmt -> Contexts Stmt
getBlockExpr (Block [])     (Block y) = return $ Block (reverse y)
getBlockExpr (Block (x:xs)) (Block y) = do
   expr <- getExpr x
   getBlockExpr (Block xs) (Block (expr : y))
getBlockExpr _              _         = undefined

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

eng :: String
eng = ['A'..'Z']  -- Arbitrary letters, is there a more standard way to do this?

base26 :: Contexts String  -- Need to edit this
base26 = do 
   i <- get
   inc
   case i of 
      0 -> return [head eng]
      n -> return $ reverse (f n)
         where
            base = length eng
            f 0 = []
            f x =
               let (q, r) = (x - 1) `divMod` base
               in (eng !! r) : f q

genVar :: Contexts Ident
genVar = do
   ctx <- askC
   var <- base26
   case M.lookup var ctx of
      Just x  -> do
         genVar
      Nothing -> do 
         return var

getVar :: Ident -> Contexts Ident
getVar v = Data.Maybe.fromMaybe v . M.lookup v <$> askV


-- Type checking prog:
---------------------------------------------------------------------
checkProg :: Prog -> Either TypeError Prog
checkProg p = do
   (prog, _) <- runEnv (M.empty, M.empty, M.empty) 0 (testProg p)
   Right $ reverse prog

-- I do not recall why this is here, doesn't feel necessary but also very well could be 
testProg :: Prog -> Contexts Prog
testProg p = elaborateProg p []

-- I considered making this `elaborateProg :: Stmt -> Contexts Stmt` so that I could use map with it instead of manual recursing through
   -- Decided against it because there would be cases in which I need to split a statement into two statements
elaborateProg :: Prog -> Prog -> Contexts Prog
elaborateProg []                  prog = return prog
elaborateProg (Assign x e : rest) prog = do
   e' <- getVars e
   ty <- infer e'
      -- `x` could be undefined, reassigned, or redefined
   elabAssign (Assign x e' : rest) prog ty (Declare ty [] (Just e')) (Assign [] e')  
elaborateProg (Block s : rest) prog   = do
   s' <- elaborateProg s []
   elaborateProg rest (Block s' : prog)
elaborateProg (IfThen e s : rest) prog   = 
   case s of 
      Block s' -> do
         e' <- getVars e
         s'' <- elaborateProg s' []
         elaborateProg rest (IfThen e' (Block s'') : prog)
      other    -> error $ "Expected block but found the following instead: " ++ show other
elaborateProg (IfElse e s o : rest) prog = 
   case s of 
      Block s1 ->  do
         e' <- getVars e
         s2 <- elaborateProg s1 []
         s3 <- elaborateProg [o] []
         case s3 of
            [x] -> elaborateProg rest (IfElse e' (Block s2) x : prog)
            _   -> error "oops in elaborateProg"
      other    -> error $ "Expected block but found the following instead: " ++ show other
elaborateProg (While e (Block s) : rest) prog = do
   e' <- getVars e
   s' <- elaborateProg s []
   while <- getExpr $ While e' (Block s')
   elaborateProg rest (while : prog)
elaborateProg (FuncDecl v args ty (Block s) : rest) prog = do 
   local (\(ctx,sCtx,vCtx) -> (M.empty, M.empty, M.empty)) $ do  -- maybe M.empty contexts
      s' <- elaborateProg s []
      elaborateProg rest (FuncDecl v args ty (Block $ reverse s') : prog)
elaborateProg (stmt : rest) prog      = do
   s' <- getExpr stmt
   elaborateProg rest (s' : prog)

-- `elabAssign` is needed for a few reasons:
   -- 1. Languages that allow the types of variables to be mutated require a lot of special effort to transcribe
   -- 2. Assignments before declarations are fine in languages like Python, but not in Java 
   -- 3. More probably but I'm sleepy
elabAssign :: Prog -> Prog -> Type -> Stmt -> Stmt -> Contexts Prog
elabAssign (Assign [] e : rest) nProg _ (Declare tyD eD e') (Assign eA e'')
   | null eD && null eA = elaborateProg rest nProg
   | null eD            = elaborateProg rest (Assign (reverse eA) e'' : nProg)
   | null eA            = elaborateProg rest (Declare tyD (reverse eD) e' : nProg)
   | otherwise          = elaborateProg rest (Declare tyD (reverse eD) e' : Assign (reverse eA) e'' : nProg)
elabAssign (Assign (x:xs) e : rest) nProg ty da a = do
   ctx <- askC
   case M.lookup x ctx of
         -- Assignment before declaration is fine in Python, but need to add the declaration for explicit languages
      Nothing -> local (\(ctx, sCtx, vCtx) -> (M.insert x ty ctx, sCtx, vCtx)) 
         $ elabAssign (Assign xs e : rest) nProg ty (addVar da x) a
      Just y  -> elabAssign' (Assign (x:xs) e : rest) nProg ty y da a
elabAssign other nProg _ _ _ = elaborateProg other nProg

elabAssign' :: Prog -> Prog -> Type -> Type -> Stmt -> Stmt -> Contexts Prog
elabAssign' (Assign (x:xs) e : rest) nProg ty y da a = do 
   vCtx <- askV 
   if x `elem` M.elems vCtx
   then do 
      x' <- genVar
      local (\(ctx, sCtx, vCtx) -> (M.insert x' ty ctx, sCtx, M.insert x x' vCtx)) 
         $ elabAssign (Assign xs e : rest) nProg ty (addVar da x') a
   else if y == ty 
   then local (\(ctx, sCtx, vCtx) -> (ctx, sCtx, M.delete x vCtx)) 
      $ elabAssign (Assign xs e : rest) nProg ty da (addVar a x)
   else do
      sCtx <- askS
      case M.lookup x sCtx of
         Just vList -> do
            subV <- findSubVariable ty vList
            case subV of
               Just s  -> local (\(ctx, sCtx, vCtx) -> (ctx, sCtx, M.insert x s vCtx)) 
                  $ elabAssign (Assign xs e : rest) nProg ty da (addVar a s)
               Nothing -> do
                  x' <- genVar
                  local (\(ctx, sCtx, vCtx) -> (M.insert x' ty ctx, M.insert x (x' : vList) sCtx, M.insert x x' vCtx)) 
                     $ elabAssign (Assign xs e : rest) nProg ty (addVar da x') a
         Nothing    -> do
            x' <- genVar
            local (\(ctx, sCtx, vCtx) -> (M.insert x' ty ctx, M.insert x [x'] sCtx, M.insert x x' vCtx)) 
               $ elabAssign (Assign xs e : rest) nProg ty (addVar da x') a
elabAssign' _ _ _ _ _ _ = undefined

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
infer (Call v e)       = do 
   e' <- mapM infer e 
   return Poly
infer x                = error $ "The following expression is not yet implemented: " ++ show x -- so this should show the actual expression

inferArray :: Type -> [Expr] -> Contexts Type -- Not sure what is going on here, VSCode recommended changes
inferArray ty = foldr
      (\ x
         -> (*>)
              (do ctx <- askC
                  check x ty))
      (return (TyArr ty))

-- inferArgs :: [Args] -> [Args] -> Contexts [Args]
-- inferArgs []       args = return $ reverse args 
-- inferArgs ((Args _ v):rest) args = do 
--    ctx <- askC 
--    case M.lookup v ctx of 
--       Nothing -> error $ "Bad inference. Idiot. " ++ show v 
--       Just ty -> inferArgs rest (Args ty (reverse v) : args) 


check :: Expr -> Type -> Contexts ()
check e ty = do
   ty' <- infer e
   if ty == ty'
   then return ()
   else throwError $ TypeMismatch ty ty'

