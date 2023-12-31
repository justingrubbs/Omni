

module Deprecated.Inference
   ()
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


-- Variable Generation and Maintenance:
---------------------------------------------------------------------
getExpr :: Stmt -> Contexts Stmt
getExpr (Assign x e)        = Assign x <$> getVars e
getExpr (AugAssign v a e)   = AugAssign v a <$> getVars e
getExpr (Declare ty x e)    =
   case e of
      Nothing -> return $ Declare ty x e
      Just e' -> do
         expr <- getVars e'
         return $ Declare ty x (Just expr)
getExpr (IfThen e s)        = IfThen <$> getVars e <*> getExpr s
getExpr (IfElse e s1 s2)    = IfElse <$> getVars e <*> getExpr s1 <*> getExpr s2
getExpr (While e s)         = While <$> getVars e <*> getExpr s
getExpr (Block s)           = Block <$> mapM getExpr s
getExpr (ExprStmt e)        = ExprStmt <$> getVars e
getExpr (FuncDecl v e ty s) = return $ FuncDecl v e ty s
getExpr (Return e)          = 
   case e of 
      Nothing -> return $ Return e
      Just e' -> do 
         expr <- getVars e' 
         return $ Return (Just expr)
getExpr (OtherS s)          = return $ OtherS s
-- getExpr x                   = error $ show x ++ " was not intended to be in prog"

getVars :: Expr -> Contexts Expr
getVars (Var x)        = Var <$> getVar x
getVars (Bin op e1 e2) = Bin op <$> getVars e1 <*> getVars e2
getVars (Un op e1)     = Un op <$> getVars e1
getVars (Array x)      = Array <$> mapM getVars x
getVars (ArrayA x ty)  = do 
   arr <- mapM getVars x 
   return $ ArrayA arr ty
getVars (Call v e)     = Call v <$> mapM getVars e
getVars (Output e)     = Output <$> getVars e
getVars x              = return x

eng :: String
eng = ['A'..'Z']  -- Arbitrary letters, is there a more standard way to do this?

base26 :: Contexts String
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
               if i < 26 
               then let (q, r) = x `divMod` base
                  in (eng !! r) : f q
               else let (q, r) = (x - 1) `divMod` base
                  in (eng !! r) : f q

genVar :: Contexts Ident
genVar = do
   ctx <- askC
   var <- base26
   case M.lookup var ctx of
      Just x  -> genVar
      Nothing -> return var

getVar :: Ident -> Contexts Ident
getVar v = Data.Maybe.fromMaybe v . M.lookup v <$> askV


-- Type checking prog:
---------------------------------------------------------------------
checkProg :: Prog -> Either TypeError Prog
checkProg p = do
   (prog, _) <- runEnv (M.empty, M.empty, M.empty) 0 (elabProg p [])
   Right $ reverse prog

-- I considered making this `elabProg :: Stmt -> Contexts Stmt` so that I could use map with it instead of manual recursing through
   -- Decided against it because there would be cases in which I need to split a statement into two statements
elabProg :: Prog -> Prog -> Contexts Prog
elabProg []                  prog = return prog
elabProg (Assign x e : rest) prog = do
   e' <- getVars e
   ty <- infer e'
   case e' of 
      Array arr -> elabAssign (Assign x (ArrayA arr ty) : rest) prog ty (Declare ty [] (Just e')) (Assign [] (ArrayA arr ty))
      _         -> elabAssign (Assign x e' : rest) prog ty (Declare ty [] (Just e')) (Assign [] e') 
elabProg (AugAssign v a e : rest) prog = do 
   e' <- getVars e 
   ty <- infer e' 
   elabAugAssign (AugAssign v a e' : rest) prog ty
elabProg (Block s : rest) prog   = do
   s' <- elabProg s []
   elabProg rest (Block s' : prog)
elabProg (IfThen e (Block s) : rest) prog = do
   e' <- getVars e
   s' <- elabProg s []
   (prog', new) <- checkScope s' prog []
   elabProg rest (IfThen e' (Block (reverse new)) : prog')
-- Need to finish this checkScope
elabProg (IfElse e s o : rest) prog = 
   case s of 
      Block s1 -> do
         e' <- getVars e
         s2 <- elabProg s1 []
         s3 <- elabProg [o] []
         case s3 of
            [x] -> elabProg rest (IfElse e' (Block (reverse s2)) x : prog)
            _   -> error "oops in elabProg"
      other    -> error $ "Expected block but found the following instead: " ++ show other
elabProg (While e (Block s) : rest) prog = do
   e' <- getVars e
   s' <- elabProg s []
   (prog', new) <- checkScope s' prog []
   while <- getExpr $ While e' (Block (reverse new))
   elabProg rest (while : prog')
elabProg (FuncDecl v args ty (Block s) : rest) prog = do  -- maybe M.empty contexts
   s' <- elabProg s []
   elabProg rest (FuncDecl v args ty (Block $ reverse s') : prog)
elabProg (stmt : rest) prog      = do
   s' <- getExpr stmt
   elabProg rest (s' : prog)

-- `elabAssign` is needed for a few reasons:
   -- 1. Languages that allow the types of variables to be mutated require a lot of special effort to transcribe
   -- 2. Assignments before declarations are fine in languages like Python, but not in Java 
   -- 3. More probably but I'm sleepy
elabAssign :: Prog -> Prog -> Type -> Stmt -> Stmt -> Contexts Prog
elabAssign (Assign [] e : rest) nProg _ (Declare tyD eD e') (Assign eA e'')
   | null eD && null eA = elabProg rest nProg
   | null eD            = elabProg rest (Assign (reverse eA) e'' : nProg)
   | null eA            = elabProg rest (Declare tyD (reverse eD) e' : nProg)
   | otherwise          = elabProg rest (Declare tyD (reverse eD) e' : Assign (reverse eA) e'' : nProg)
elabAssign (Assign (x:xs) e : rest) nProg ty da a = do
   ctx <- askC
   case M.lookup x ctx of
         -- Assignment before declaration is fine in Python, but need to add the declaration for explicit languages
      Nothing -> local (\(ctx, sCtx, vCtx) -> (M.insert x ty ctx, sCtx, vCtx)) 
         $ elabAssign (Assign xs e : rest) nProg ty (addVar da x) a
      Just y  -> elabAssign' (Assign (x:xs) e : rest) nProg ty y da a
elabAssign other nProg _ _ _ = elabProg other nProg

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

elabAugAssign :: Prog -> Prog -> Type -> Contexts Prog 
elabAugAssign (AugAssign v a e : rest) nProg ty = do 
   ctx <- askC 
   case M.lookup v ctx of 
      Nothing -> throwError $ UndefinedVar v
      Just x  -> do 
         vCtx <- askV 
         case M.lookup v vCtx of 
            Nothing -> 
               if ty == x 
               then elabProg rest (AugAssign v a e : nProg) 
               else throwError $ TypeMismatch ty x
            Just y  -> elabAugAssign (AugAssign y a e : rest) nProg ty
elabAugAssign _ _ _ = error "Impossible pattern match fail in elabAugAssign"

checkScope :: Prog -> Prog -> Prog -> Contexts (Prog, Prog) 
checkScope []                    prog new = return (prog, reverse new)
checkScope (Declare ty v e:rest) prog new = case e of 
   Nothing -> checkScope rest (Declare ty v e : prog) new 
   Just e' -> case e' of 
      Array arr -> checkScope rest (Declare ty v (Just $ Lit $ baseValue ty) : prog) (Assign v (ArrayA arr ty) : new)
      _         -> checkScope rest (Declare ty v (Just $ Lit $ baseValue ty) : prog) (Assign v e' : new)
checkScope (s:rest)              prog new = checkScope rest prog (s:new)

baseValue :: Type -> Literal 
baseValue TyInt      = Int 0
baseValue TyFloat    = Float 0
baseValue TyDouble   = Double 0 
baseValue TyBool     = Bool True 
baseValue TyChar     = Char ' '
baseValue TyStr      = Str ""
baseValue (TyArr x)  = Null 
baseValue Poly       = Null
baseValue (OtherT t) = error $ "Bad type: " ++ show t
baseValue rest       = error $ "Unmatched pattern in baseValue: " ++ show rest

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
infer (Lit (Str [s]))  = return TyStr
infer (Lit (Str s))    = return TyStr
infer (Var v)          = do
   ctx <- askC
   case M.lookup v ctx of
      Nothing -> throwError $ UndefinedVar v
      Just x  -> return x
infer (Bin op e1 e2)   = do
   e1' <- infer e1
   check e2 e1' *> case op of
      And -> check e1 TyBool $> TyBool
      Or  -> check e1 TyBool $> TyBool
      Eq  -> return TyBool
      NEq -> return TyBool
      Less -> return TyBool
      Greater -> return TyBool
      LessEq -> return TyBool
      GreaterEq -> return TyBool
      -- Mul -> Can be num -> num -> num or string -> num -> [string?] regardless, need to allow for two different types      
      Add -> case e1' of 
         TyInt -> return TyInt 
         TyStr -> return TyStr 
         _     -> do 
            e2' <- infer e2 
            error $ show $ Bin op e1 e2
            -- throwError $ TypeMismatch e1' e2'
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
infer (ArrayA x ty)    = return ty
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
   else error $ show e
      -- throwError $ TypeMismatch ty ty'

