

module Omni.Typecheck.Elaborate
   (
      checkProg
   )
   where


import            Omni.Data
import            Omni.Typecheck.Data
import            Omni.Typecheck.Inference   (inferStmt, checkStmt, infer, check)
import            Omni.Typecheck.Constraints
import qualified  Data.Map                   as M
import            Debug.Trace
import            Data.Maybe                 (fromMaybe)
import            Control.Monad.State
import            Control.Monad.Reader
import            Control.Monad.Writer
import            Control.Monad.Except
import            Control.Monad.Identity
import            Data.Functor


-- Checking (and modifying) progression of statements:
---------------------------------------------------------------------
checkProg :: Prog -> Either TypeError Prog
checkProg p = do
   (prog, _) <- runEnv empty 0 (elabProg p [])
   Right $ reverse prog

end :: Prog -> Contexts (Prog, Env)
end prog = do
   env <- ask
   return (prog,env)

elabProg :: Prog -> Prog -> Contexts Prog
elabProg []                         prog = return prog  -- Successful elaboration
elabProg (FuncDecl v a ty s : rest) prog = do 
   (sProg,fEnv) <- local (const empty) $ elabStmt (FuncDecl v a ty s)  -- It's not obvious whether this should be local or not (see elab.java)
   local id $ elabProg rest (sProg ++ prog)
elabProg (s:rest)                   prog = do
   (sProg, env) <- elabStmt s
   local (const env) $ elabProg rest (sProg ++ prog)

elabStmt :: Stmt -> Contexts (Prog,Env)
elabStmt stmt = do 
   s' <- getExpr stmt
   checkStmt s' *> elabStmt' s'

elabStmt' :: Stmt -> Contexts (Prog, Env)
elabStmt' (Assign v e)        = do
   ty <- infer e
   let sTuple = helpAssign e ty
   let e' = elabExpr e ty
   elabAssign (Assign v e') ty [] sTuple
elabStmt' (AugAssign v a e)   = do
   ty <- infer e
   elabAugAssign (AugAssign v a e) ty
elabStmt' (Block p)           = elabBlock p []
elabStmt' (IfThen e s)        = elabIfThen (IfThen e s)
elabStmt' (IfElse e s1 s2)    = elabIfElse (IfElse e s1 s2)
elabStmt' (While e s)         = elabWhile (While e s)
elabStmt' (FuncDecl v a ty s) = elabFuncDecl (FuncDecl v a ty s)
elabStmt' other = do 
   env <- ask 
   return ([other],env)


-- Elaborating assignments:
---------------------------------------------------------------------
elabAssign :: Stmt -> Type -> Prog -> (Stmt, Stmt) -> Contexts (Prog, Env)
elabAssign (Assign [] e) ty prog (Declare tyD v1 e1, Assign v2 e2)
   | null v1 && null v2 = end prog
   | null v1            = end (Assign (reverse v2) e2 : prog)
   | null v2            = end (Declare tyD (reverse v1) e1 : prog)
   | otherwise          = end (Declare tyD (reverse v1) e1 : Assign (reverse v2) e2 : prog)
elabAssign (Assign (x:rest) e) ty prog (decl, asgn) = do
   ctx <- askC
   case M.lookup x ctx of
      Nothing -> local (\(ctx, sCtx, vCtx) -> (M.insert x ty ctx, sCtx, vCtx))
         $ elabAssign (Assign rest e) ty prog (addVar decl x, asgn)
      Just y  -> elabAssign' (Assign (x:rest) e) ty y prog (decl, asgn)
elabAssign _ _ _ _ = undefined

elabAssign' :: Stmt -> Type -> Type -> Prog -> (Stmt, Stmt) -> Contexts (Prog, Env)
elabAssign' (Assign (x:rest) e) ty1 ty2 prog (decl,asgn) = do
   vCtx <- askV
   if x `elem` M.elems vCtx
   then do
      x' <- genVar
      local (\(ctx, sCtx, vCtx) -> (M.insert x' ty1 ctx, sCtx, M.insert x x' vCtx))
         $ elabAssign (Assign rest e) ty1 prog (addVar decl x', asgn)
   else if ty1 == ty2
   then local (\(ctx, sCtx, vCtx) -> (ctx, sCtx, M.delete x vCtx))
      $ elabAssign (Assign rest e) ty1 prog (decl, addVar asgn x)
   else do
      sCtx <- askS
      case M.lookup x sCtx of
         Just vList -> do
            subV <- findSubVariable ty1 vList
            case subV of
               Just s  -> local (\(ctx, sCtx, vCtx) -> (ctx, sCtx, M.insert x s vCtx))
                  $ elabAssign (Assign rest e) ty1 prog (decl, addVar asgn s)
               Nothing -> do
                  x' <- genVar
                  local (\(ctx, sCtx, vCtx) -> (M.insert x' ty1 ctx, M.insert x (x' : vList) sCtx, M.insert x x' vCtx))
                     $ elabAssign (Assign rest e) ty1 prog (addVar decl x', asgn)
         Nothing    -> do
            x' <- genVar
            local (\(ctx, sCtx, vCtx) -> (M.insert x' ty1 ctx, M.insert x [x'] sCtx, M.insert x x' vCtx))
               $ elabAssign (Assign rest e) ty1 prog (addVar decl x', asgn)
elabAssign' _ _ _ _ _ = undefined

helpAssign :: Expr -> Type -> (Stmt, Stmt)
helpAssign e ty = (Declare ty [] (Just e), Assign [] (elabExpr e ty))

elabExpr :: Expr -> Type -> Expr
elabExpr (Array arr) ty = ArrayA arr ty
elabExpr rest        _  = rest

addVar :: Stmt -> Ident -> Stmt
addVar (Declare ty x (Just e)) var = Declare ty (var : x) (Just e)
addVar (Assign x e)            var = Assign (var : x) e
addVar rest                    _   = error ("Unexpected statement in addVar: " ++ show rest)


-- Elaborating augmented assignments:
---------------------------------------------------------------------
elabAugAssign :: Stmt -> Type -> Contexts (Prog,Env)
elabAugAssign (AugAssign v a e) ty = do
   ctx <- askC
   case M.lookup v ctx of
      Nothing -> throwError $ UndefinedVar v
      Just x  -> do
         vCtx <- askV
         case M.lookup v vCtx of
            Nothing ->
               if ty == x
               then end [AugAssign v a e]
               else throwError $ TypeMismatch ty x
            Just y  -> elabAugAssign (AugAssign y a e) ty
elabAugAssign _ _ = undefined


-- Elaborating block:
---------------------------------------------------------------------
elabBlock :: Prog -> Prog -> Contexts (Prog, Env)
elabBlock []       prog = end prog  -- I am not sure whether the block should be included in the overall progression -- local id ?
elabBlock (s:rest) prog = do
   (sProg, env) <- elabStmt s
   local (const env) $ elabBlock rest (sProg ++ prog)


-- Elaborating conditionals and loops:
---------------------------------------------------------------------
elabIfThen :: Stmt -> Contexts (Prog, Env)
elabIfThen (IfThen e s) = check e TyBool *> do
   (sProg,env) <- elabStmt s
   let (sProg',pref) = fixScope sProg [] []
   local (const env) $ end (IfThen e (Block sProg') : pref)
elabIfThen _ = undefined

-- Nests it instead of using `else if {}` but not the end of the world
elabIfElse :: Stmt -> Contexts (Prog,Env)
elabIfElse (IfElse e s1 s2) = check e TyBool *> do 
   (sProg1,env1) <- elabStmt s1 
   (sProg2,env2) <- local (const env1) $ elabStmt s2  -- Should the branches of IfElse be in same scope? Probably?
   let (sProg1',pref1) = fixScope sProg1 [] [] 
   let (sProg2',pref2) = fixScope sProg2 [] [] 
   local (const env2) $ end (IfElse e (Block sProg1') (Block sProg2') : pref1 ++ pref2)
elabIfElse _ = undefined

elabWhile :: Stmt -> Contexts (Prog,Env)
elabWhile (While e s) = do 
   (sProg,env) <- elabStmt s  
   let (sProg',pref) = fixScope sProg [] [] 
   local (const env) $ end (While e (Block sProg') : pref)
elabWhile _ = undefined

fixScope :: Prog -> Prog -> Prog -> (Prog, Prog)
fixScope []                      prog pref = (prog,pref)
fixScope (Declare ty v e : rest) prog pref = case e of
   Nothing          -> fixScope rest prog (Declare ty v e : pref)
   Just (Array arr) -> fixScope rest (Assign v (ArrayA arr ty) : prog) (Declare ty v (Just $ Lit $ baseValue ty) : pref)
   -- Just e'          -> error "Java Conversion Error: \nBecause the scope of a variable declaration \
   -- \is local to the block it is in, primitive declarations must be directly in the body of a function\n"
   Just e'          -> fixScope rest (Assign v e' : prog) (Declare ty v (Just $ Lit $ baseValue ty) : pref)
fixScope (s:rest)                prog pref = fixScope rest (s : prog) pref

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


-- Elaborating function declarations:
---------------------------------------------------------------------
elabFuncDecl :: Stmt -> Contexts (Prog,Env)
elabFuncDecl (FuncDecl "main" a ty s) = 
   if null a 
   then do 
      (sProg,env) <- elabStmt s 
      ty' <- local (const env) $ inferStmt (FuncDecl "main" a ty (Block sProg))
      case ty' of 
         TyVoid -> end [FuncDecl "main" [Args (TyArr TyStr) "args"] TyVoid (Block $ reverse sProg)]
         _      -> throwError $ MainType "main" ty'
   else throwError $ MainArgs "main" a 
elabFuncDecl (FuncDecl v a ty s)      = do 
   (sProg,env) <- elabStmt s
   ty' <- local (const env) $ inferStmt (FuncDecl v a ty (Block sProg))
   end [FuncDecl v a ty' (Block $ reverse sProg)]
elabFuncDecl _ = undefined


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

