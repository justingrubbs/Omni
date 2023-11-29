

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
   _ -> throwError $ STypeMismatch ty Num  -- Should not be just ty int but fine
subtype ty Text = case ty of 
   TyStr  -> return () 
   TyChar -> return () 
   _ -> throwError $ STypeMismatch ty Text 
subtype ty None = throwError $ STypeMismatch ty None


-- Inferring the types of statements:
---------------------------------------------------------------------
-- Check that the statements are well-typed
   -- Checking occurs before elaboration
checkStmt :: Stmt -> Contexts ()
checkStmt (AugAssign v a e)   = do 
   e' <- infer e 
   subtype e' Num 
checkStmt (IfThen e s)        = check e TyBool
checkStmt (IfElse e s1 s2)    = check e TyBool
checkStmt (While e s)         = check e TyBool
checkStmt (Return (Just e))   = infer e $> ()
checkStmt _                   = return ()

-- Infer the type of *some* statements
   -- Inference on statements occurs after elaboration
inferStmt :: Stmt -> Contexts Type
inferStmt (FuncDecl v a ty (Block s)) = inferFuncDecl s Nothing
inferStmt (ExprStmt (Call v e))       = undefined 
inferStmt (ExprStmt _)                = return TyVoid
inferStmt rest                        = error $ "Unmatched pattern in inferStmt: " ++ show rest



-- Inference on function declarations:
---------------------------------------------------------------------
inferFuncDecl :: Prog -> Maybe Type -> Contexts Type
inferFuncDecl []       Nothing      = return TyVoid
inferFuncDecl []       (Just ty)    = return ty
inferFuncDecl (Return e : rest) mty = case e of
   Nothing -> inferFuncDecl rest mty
   Just e  -> case mty of
      Nothing -> do
         eTy <- infer e
         inferFuncDecl rest (Just eTy)
      Just ty -> check e ty *> inferFuncDecl rest mty
inferFuncDecl (s:rest) mty          = inferFuncDecl rest mty

-- inferArgs :: [Args] -> [Args] -> Contexts [Args]
-- inferArgs []       args = return $ reverse args 
-- inferArgs ((Args _ v):rest) args = do 
--    ctx <- askC 
--    case M.lookup v ctx of 
--       Nothing -> error $ "Bad inference. Idiot. " ++ show v 
--       Just ty -> inferArgs rest (Args ty (reverse v) : args)


-- Inferring the types of expressions:
---------------------------------------------------------------------
check :: Expr -> Type -> Contexts ()
check e ty = do
   ty' <- infer e
   if ty == ty'
   then return ()
   else throwError $ TypeMismatch ty ty'

infer :: Expr -> Contexts Type
infer (Lit l)          = inferLit l
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

inferLit :: Literal -> Contexts Type
inferLit (Int n)    = return TyInt
inferLit (Float f)  = return TyFloat
inferLit (Double d) = return TyDouble
inferLit (Bool b)   = return TyBool
inferLit (Char c)   = return TyChar
inferLit (Str [s])  = return TyStr
inferLit (Str s)    = return TyStr
inferLit (OtherL l) = error $ "The following literal is not yet implemented: " ++ show l
inferLit Null       = return TyVoid -- Unsure

inferArray :: Type -> [Expr] -> Contexts Type -- Not sure what is going on here, VSCode recommended changes
inferArray ty = foldr
      (\ x
         -> (*>)
              (do ctx <- askC
                  check x ty))
      (return (TyArr ty))

