

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


-- Testing:
---------------------------------------------------------------------
testProg :: Prog
testProg = [FunDecl "main" [] Poly (Block [Assign ["a"] (Call ["add"] [Lit (Int 3),Lit (Int 2)]),Assign ["b"] (Call ["mul"] [Var "a",Lit (Int 2)]),ExprStmt (Output (Var "a")),ExprStmt (Output (Var "b")),ExprStmt (Output (Call ["mul"] [Var "a",Var "b"]))]),FunDecl "add" [Args Poly "x",Args Poly "y"] Poly (Block [Return (Just (Bin Add (Var "x") (Var "y")))]),FunDecl "mul" [Args Poly "x",Args Poly "y"] Poly (Block [Return (Just (Bin Mul (Var "x") (Var "y")))]),ExprStmt (Call ["main"] [])]

run :: Either TypeError Prog
run = checkProg testProg


-- Checking (and modifying) progression of statements:
---------------------------------------------------------------------
checkProg :: Prog -> Either TypeError Prog
checkProg p = do
   prog <- runEnv empty 0 (prepFuncs p [])
   Right $ reverse prog

end :: Prog -> Contexts (Prog, Env)
end prog = do
   env <- ask
   return (prog,env)

prepFuncs :: Prog -> Prog -> Contexts Prog
prepFuncs []                        prog = do
   p <- elabProg False prog []
   elabProg True p []
prepFuncs (FunDecl v a ty s : rest) prog = do
   putStmt v (FunDecl v a ty s)
   putEnv v empty
   prepFuncs rest (FunDecl v a ty s : prog)
prepFuncs (stmt : rest)             prog = prepFuncs rest (stmt : prog)

elabProg :: Bool -> Prog -> Prog -> Contexts Prog
elabProg b    []                        prog = return prog  -- Successful elaboration
elabProg True (FunDecl v a ty s : rest) prog = do
   env <- getEnv v
   -- (sProg,fEnv) <- local (const env) $ elabStmt $ FunDecl v a ty s
   (sProg,fEnv) <- elabStmt $ FunDecl v a ty s
   elabProg True rest (sProg ++ prog)
elabProg b    (FunDecl v a ty s : rest) prog = do
   env <- getEnv v
   -- (sProg,fEnv) <- local (const env) $ elabStmt (FunDecl v a ty s) `catchError` (\te -> return ([FunDecl v a ty s],stripError te))
   (sProg,fEnv) <- elabStmt (FunDecl v a ty s) `catchError` (\te -> return ([FunDecl v a ty s],stripError te))
   elabProg b rest (sProg ++ prog)
elabProg b    (s:rest)                  prog = do
   (sProg, env) <- elabStmt s
   elabProg b rest (sProg ++ prog)

elabStmt :: Stmt -> Contexts (Prog,Env)
elabStmt stmt = do
   s' <- getExpr stmt
   checkStmt s' *> elabStmt' s'

elabStmt' :: Stmt -> Contexts (Prog, Env)
elabStmt' (Assign v e)       = do
   expr <- elabExpr e
   ty <- infer expr
   let sTuple = helpAssign expr ty
   let e' = elabArray expr ty
   elabAssign (Assign v e') ty [] sTuple
elabStmt' (AugAssign v a e)  = do
   expr <- elabExpr e
   ty <- infer expr
   elabAugAssign (AugAssign v a expr) ty
elabStmt' (Declare ty v e)   = case e of
   Nothing -> do end [Declare ty v e]
   Just e' -> do
      expr <- elabExpr e'
      eTy <- infer expr
      end [Declare eTy v (Just expr)]
elabStmt' (Block p)          = elabBlock p []
elabStmt' (IfThen e s)       = do
   expr <- elabExpr e
   elabIfThen (IfThen expr s)
elabStmt' (IfElse e s1 s2)   = do
   expr <- elabExpr e
   elabIfElse (IfElse expr s1 s2)
elabStmt' (While e s)        = do
   expr <- elabExpr e
   elabWhile (While expr s)
elabStmt' (FunDecl v [] ty s) = do 
   env <- getEnv v 
   stmt <- getStmt v 
   local (const env) $ elabFunDecl stmt
elabStmt' (FunDecl v a ty s) = do 
   stmt <- getStmt v
   end [stmt]
elabStmt' (ExprStmt e)      = do
   vCtx <- askV
   trace (show $ ExprStmt e) $ pure ()
   trace (show vCtx) $ pure ()
   expr <- elabExpr e
   end [ExprStmt expr]
elabStmt' (Return e)        = case e of
   Nothing -> end [Return e]
   Just e' -> do
      expr <- elabExpr e'
      end [Return (Just expr)]
elabStmt' (OtherS s)         = error $ show s


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
elabAssign' (Assign (x:rest) e) ty1 ty2 prog (decl,asgn)
   | ty1 == ty2 = local (\(ctx, sCtx, vCtx) -> (ctx, sCtx, M.delete x vCtx))
      $ elabAssign (Assign rest e) ty1 prog (decl, addVar asgn x)
   | otherwise = do
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
helpAssign e ty = (Declare ty [] (Just e), Assign [] (elabArray e ty))

elabArray :: Expr -> Type -> Expr
elabArray (Array arr) ty = ArrayA arr ty
elabArray rest        _  = rest

addVar :: Stmt -> Ident -> Stmt
addVar (Declare ty x (Just e)) var = Declare ty (var : x) (Just e)
addVar (Assign x e)            var = Assign (var : x) e
addVar rest                    _   = error ("Unexpected statement in addVar: " ++ show rest)


-- Elaborating augmented assignments:
---------------------------------------------------------------------
elabAugAssign :: Stmt -> Type -> Contexts (Prog,Env)
elabAugAssign (AugAssign v a e) ty = do
   ctx <- askC
   env <- ask
   case M.lookup v ctx of
      Nothing -> throwError $ UndefinedVar v env
      Just x  -> do
         vCtx <- askV
         case M.lookup v vCtx of
            Nothing ->
               if ty == x
               then end [AugAssign v a e]
               else throwError $ TypeMismatch ty x env
            Just y  -> elabAugAssign (AugAssign y a e) ty
elabAugAssign _ _ = undefined


-- Elaborating blocks:
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
   (sProg2,env2) <- local (const env1) $ elabStmt s2
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
   Just e'          -> error "Java Conversion Error: \nBecause the scope of a variable declaration \
   \is local to the block it is in, primitive declarations must be directly in the body of a function\n"
   -- Just e'          -> fixScope rest (Assign v e' : prog) (Declare ty v (Just $ Lit $ baseValue ty) : pref)
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


-- Elaborating functions:
---------------------------------------------------------------------
elabFunDecl :: Stmt -> Contexts (Prog,Env)
elabFunDecl (FunDecl "main" a t s) =
   if null a
   then do
      (sProg,env') <- elabStmt s
      ty <- local (const env') $ inferStmt (FunDecl "main" a t (Block sProg))
      putEnv "main" env'
      case ty of
         TyVoid -> do 
            let f = FunDecl "main" [Args (TyArr TyStr) "args"] ty (Block $ reverse sProg)
            putStmt "main" f
            end [f]
         _      -> throwError $ MainType "main" ty env'
   else throwError $ MainArgs "main" a empty
elabFunDecl (FunDecl v a t s)     = do
   (sProg,env') <- elabStmt s
   ty <- local (const env') $ inferStmt (FunDecl v a t (Block sProg))
   let (ctx,sCtx,vCtx) = env' 
   putEnv v (M.insert v ty ctx, sCtx,vCtx)
   putStmt v (FunDecl v a ty (Block $ reverse sProg))
   end [FunDecl v a ty (Block $ reverse sProg)]
elabFunDecl _ = undefined

stripError :: TypeError -> Env
stripError (TypeMismatch t1 t2 env) = env
stripError (STypeMismatch t1 t2 env) = env
stripError (UndefinedVar v env) = env
stripError (DuplicateVar v env) = env
stripError (BadVoid e env) = env
stripError (VoidArray a env) = env
stripError (UndefinedFun v env) = env
stripError (MainType v t env) = env
stripError (MainArgs v a env) = env



-- Elaborating expressions:
---------------------------------------------------------------------
elabExpr :: Expr -> Contexts Expr
elabExpr (Call v e) = do 
   env <- getEnv (concat v) 
   -- local (const env) $ elabFunCall (Call v e)
   elabFunCall (Call v e)
elabExpr (Array e)  = elabExpr' (Array e) []
elabExpr (ArrayA e ty) = elabExpr' (ArrayA e ty) []
elabExpr (Bin op e1 e2) = do
   expr1 <- elabExpr e1
   expr2 <- elabExpr e2
   return $ Bin op expr1 expr2
elabExpr (Un aop e) = do
   expr <- elabExpr e
   return $ Un aop expr
elabExpr (Output e) = do
   expr <- elabExpr e
   return $ Output expr
elabExpr rest       = return rest

elabExpr' :: Expr -> [Expr] -> Contexts Expr
elabExpr' (Array [])       exprs = return $ Array exprs
elabExpr' (Array (e:rest)) exprs = do
   expr <- elabExpr e
   elabExpr' (Array rest) (expr : exprs)
elabExpr' (ArrayA [] ty) exprs = return $ ArrayA exprs ty
elabExpr' (ArrayA (e:rest) ty) exprs = do
   expr <- elabExpr e
   elabExpr' (ArrayA rest ty) (expr : exprs)
elabExpr' _ _ = undefined

elabFunCall :: Expr -> Contexts Expr
elabFunCall (Call ["main"] e) = return $ Call ["main"] e
elabFunCall (Call v [])       = return $ Call v []
elabFunCall (Call v e)        = do
   let v' = concat v
   env <- getEnv v'
   args <- getArgs v'
   env' <- argsToCtx env v' e args []
   stmt <- getStmt v'
   -- (sProg,env'') <- local (const env') $ elabFunDecl stmt
   -- let [s] = sProg
   -- putStmt v' s
   local (const env') $ elabFunDecl stmt
   return $ Call v e
elabFunCall _                      = undefined

argsToCtx :: Env -> Ident -> [Expr] -> [Args] -> [Args] -> Contexts Env
argsToCtx env             ident []     []             args = do
   stmt <- getStmt ident
   let (FunDecl v a ty s) = stmt
   putStmt ident (FunDecl v (reverse args) ty s)
   return env
argsToCtx (ctx,sCtx,vCtx) ident (e:es) (Args ty v:as) args = do
   eTy <- infer e
   if ty /= Poly && ty /= eTy
   then do
      stmt <- getStmt ident
      error $ show stmt ++ "\n\nNeed to make argsToCtx keep trying to see if different type arguments is also valid"
   else argsToCtx (M.insert v eTy ctx,sCtx,vCtx) ident es as (Args eTy v : args)
argsToCtx _ _ _ _ _ = undefined


-- Variable Generation and Maintenance:
---------------------------------------------------------------------
getExpr :: Stmt -> Contexts Stmt
getExpr (Assign x e)       = Assign x <$> getVars e
getExpr (AugAssign v a e)  = AugAssign v a <$> getVars e
getExpr (Declare ty x e)   =
   case e of
      Nothing -> return $ Declare ty x e
      Just e' -> do
         expr <- getVars e'
         return $ Declare ty x (Just expr)
getExpr (IfThen e s)       = IfThen <$> getVars e <*> getExpr s
getExpr (IfElse e s1 s2)   = IfElse <$> getVars e <*> getExpr s1 <*> getExpr s2
getExpr (While e s)        = While <$> getVars e <*> getExpr s
getExpr (Block s)          = Block <$> mapM getExpr s
getExpr (ExprStmt e)       = ExprStmt <$> getVars e
getExpr (FunDecl v e ty s) = return $ FunDecl v e ty s
getExpr (Return e)         =
   case e of
      Nothing -> return $ Return e
      Just e' -> do
         expr <- getVars e'
         return $ Return (Just expr)
getExpr (OtherS s)         = return $ OtherS s
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

