

module Omni.Typecheck.Inference
   (
      inferStmt, checkStmt, infer, check
   )
   where


import            Omni.Data
import            Omni.Typecheck.Data
import qualified  Data.Map          as M
import            Debug.Trace
import            Data.Maybe        (fromMaybe)
import            Control.Monad.State
import            Control.Monad.Reader
import            Control.Monad.Writer
import            Control.Monad.Except
import            Control.Monad.Identity
import            Data.Functor


subtype :: Type -> SType -> Contexts ()
subtype ty Num  = case ty of
   TyInt    -> return ()
   TyFloat  -> return ()
   TyDouble -> return ()
   _ -> do 
      env <- ask 
      throwError $ STypeMismatch ty Num env -- Should not be just ty int but fine
subtype ty Text = case ty of
   TyStr  -> return ()
   TyChar -> return ()
   _ -> do 
      env <- ask 
      throwError $ STypeMismatch ty Text env
subtype ty None = do 
   env <- ask 
   throwError $ STypeMismatch ty None env


-- Inferring the types of statements:
---------------------------------------------------------------------
-- Check that the statements are well-typed
   -- Checking occurs before elaboration
checkStmt :: Stmt -> Contexts ()
checkStmt (AugAssign v a e)   = do
   e' <- infer e
   subtype e' Num -- This is just wrong? There are other assignment operators that don't operate on numerals
checkStmt (IfThen e s)        = check e TyBool
checkStmt (IfElse e s1 s2)    = check e TyBool
checkStmt (While e s)         = check e TyBool
checkStmt (Return (Just e))   = infer e $> ()
checkStmt (ExprStmt (Output e)) = do
   e' <- infer e
   env <- ask
   when (e' == TyVoid) $ throwError $ BadVoid e env
checkStmt _                   = return ()

-- Infer the type of *some* statements
   -- Inference on statements occurs after elaboration
inferStmt :: Stmt -> Contexts Type
inferStmt (FunDecl v a ty (Block s))  = inferFuncDecl v s Nothing
inferStmt (ExprStmt (Call v e))       = infer $ Call v e
inferStmt rest                        = error $ "Unmatched pattern in inferStmt: " ++ show rest


-- Inference on function declarations:
---------------------------------------------------------------------
inferFuncDecl :: Ident -> Prog -> Maybe Type -> Contexts Type
inferFuncDecl ident []       Nothing      = return TyVoid
inferFuncDecl ident []       (Just ty)    = return ty
-- inferFuncDecl ident (ExprStmt (Call v e) : rest) mty 
--    | concat v == ident = inferFuncDecl ident rest mty  
--    | otherwise = 
inferFuncDecl ident (Return e : rest) mty = do 
   case e of
      Nothing -> inferFuncDecl ident rest mty
      Just e'  -> 
         -- case e' of 
         -- Call v exps -> 
         --    if concat v == ident 
         --    then inferFuncDecl ident rest mty 
         --    else case mty of
         --       Nothing -> do
         --          eTy <- infer e'
         --          inferFuncDecl ident rest (Just eTy)
         --       Just ty -> check e' ty *> inferFuncDecl ident rest mty 
         -- _                -> 
         case mty of
            Nothing -> do
               eTy <- infer e'
               inferFuncDecl ident rest (Just eTy)
            Just ty -> check e' ty *> inferFuncDecl ident rest mty
-- TEMPORARY - If this works it will be extended and modularized better
inferFuncDecl ident (s:rest) mty          = 
   case s of 
      Block p -> do 
         ty <- inferFuncDecl ident p mty 
         if ty == TyVoid 
         then inferFuncDecl ident rest mty
         else inferFuncDecl ident rest (Just ty)
      IfThen e s -> do 
         ty <- inferFuncDecl ident [s] mty 
         if ty == TyVoid 
         then inferFuncDecl ident rest mty
         else inferFuncDecl ident rest (Just ty)
      IfElse e s1 s2 -> do 
         ty1 <- inferFuncDecl ident [s1] mty 
         if ty1 == TyVoid 
         then do 
            ty2 <- inferFuncDecl ident [s2] mty 
            if ty2 == TyVoid 
            then inferFuncDecl ident rest mty 
            else inferFuncDecl ident rest (Just ty2)
         else do 
            ty2 <- inferFuncDecl ident rest (Just ty1) 
            if ty2 == TyVoid 
            then inferFuncDecl ident rest (Just ty1)
            else inferFuncDecl ident rest (Just ty1)
      _ -> inferFuncDecl ident rest mty


-- Inferring the types of expressions:
---------------------------------------------------------------------
check :: Expr -> Type -> Contexts ()
check e ty = do
   ty' <- infer e
   if ty == ty'
   then return ()
   else do 
      env <- ask 
      throwError $ TypeMismatch ty ty' env

infer :: Expr -> Contexts Type
infer (Lit l)          = inferLit l
infer (Var v)          = do
   ctx <- askC
   env <- ask
   case M.lookup v ctx of
      Nothing -> throwError $ UndefinedVar v env
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
infer (Array [])       = do 
   env <- ask 
   throwError $ VoidArray "Lists must be initialized with a value." env
infer (Array (x:rest)) = do
   xTy <- infer x
   inferArray xTy rest
infer (ArrayA x ty)    = return ty
infer (Call v [])      = do 
   stmt <- getStmt (concat v) 
   let (FunDecl ident a ty s) = stmt 
   return ty
infer (Call v e)       = do
   (ctx,sCtx,vCtx) <- getEnv (concat v)
   case M.lookup (concat v) ctx of
      Nothing -> return Poly
      Just x  -> return x
infer x                = error $ "The following expression is not yet implemented: " ++ show x -- so this should show the actual expression

inferLit :: Literal -> Contexts Type
inferLit (Int n)    = return TyInt
inferLit (Float f)  = return TyFloat
inferLit (Double d) = return TyDouble
inferLit (Bool b)   = return TyBool
inferLit (Char c)   = return TyChar
inferLit (Str [s])  = return TyStr
inferLit (Str s)    = return TyStr
inferLit (OtherL l) = error $ "The following literal is not yet implemented: " ++ show l
inferLit Null       = return TyVoid

inferArray :: Type -> [Expr] -> Contexts Type -- Not sure what is going on here, VSCode recommended changes
inferArray ty = foldr
      (\ x
         -> (*>)
              (do ctx <- askC
                  check x ty))
      (return (TyArr ty))

