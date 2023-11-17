

module Deprecated.Typecheck
   (
      progEdit
   )
   where


import            Omni.Data
import qualified  Data.Map as M
import            Debug.Trace


-- https://en.wikipedia.org/wiki/Comparison_of_programming_languages_by_type_system


-- Type checking:
---------------------------------------------------------------------

type Ctx  = M.Map Ident Type
type VCtx = M.Map Ident Ident
type SCtx = M.Map Ident [Ident] -- Context containing the original variable mapped to a map of its sub-variables


-- Will eventually do type reconstruction here as well
progEdit :: Prog -> Either TypeError Prog
progEdit p = do
   (prog, ctx, sCtx, vCtx) <- elaborateProg 0 M.empty M.empty M.empty [] p
   -- trace ("Original prog " ++ show p) $ pure ()
   -- trace ("New prog " ++ show prog) $ pure ()
   Right prog


-- Variable Generation and Maintenance:
---------------------------------------------------------------------

-- Not quite sure how to work functions into this
   -- Work through the statements and if a variable is encountered that is NOT one of the parameters, update it?

-- Create function that will check every statement for variables and update them according to tVar
getExpr :: VCtx -> Stmt -> Stmt
getExpr vCtx (Assign x e)     = Assign x (getVars vCtx e)
getExpr vCtx (Declare ty x e) = 
   case e of 
      Nothing -> Declare ty x e
      Just e' -> Declare ty x (Just (getVars vCtx e'))
getExpr vCtx (IfThen e s)     = IfThen (getVars vCtx e) (getExpr vCtx s)
getExpr vCtx (IfElse e s1 s2) = IfElse (getVars vCtx e) (getExpr vCtx s1) (getExpr vCtx s2)
getExpr vCtx (While e s)      = While e (getExpr vCtx s)
getExpr vCtx (Block s)        = Block (map (getExpr vCtx ) s)
getExpr vCtx (ExprStmt e)     = ExprStmt (getVars vCtx e)
getExpr vCtx (OtherS s)       = OtherS s
getExpr vCtx (FuncDecl v e ty s) = FuncDecl v e ty (getExpr vCtx s)
getExpr vCtx x                = error $ show x ++ " was not intended to be in prog"

getBlockExpr :: VCtx -> Stmt -> Stmt -> Stmt 
getBlockExpr _    (Block [])     (Block y) = Block (reverse y)
getBlockExpr vCtx (Block (x:xs)) (Block y) = getBlockExpr vCtx (Block xs) (Block (getExpr vCtx x : y))

getVars :: VCtx -> Expr -> Expr
getVars vCtx (Var x)        = Var (getVar vCtx x)
getVars vCtx (Args ty v)    = Args ty v
getVars vCtx (Bin op e1 e2) = Bin op (getVars vCtx e1) (getVars vCtx e2)
getVars vCtx (Un op e1)     = Un op (getVars vCtx e1)
getVars vCtx (Array x)      = Array (map (getVars vCtx) x)
getVars vCtx (Output e)     = Output (getVars vCtx e)
getVars _    x              = x

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

genVar :: Int -> Ctx -> (Ident, Int)
genVar n ctx =
   let var = base26 n
   in case M.lookup var ctx of
         Just x  -> genVar (n + 1) ctx
         Nothing -> (var, n)

-- This is necessary because Python has mutable variables and types
getVar :: VCtx -> Ident -> Ident
getVar vCtx v =
   case M.lookup v vCtx of
      Just x  -> x
      Nothing -> v

elaborateProg :: Int -> Ctx -> SCtx -> VCtx -> Prog -> Prog -> Either TypeError (Prog, Ctx, SCtx, VCtx)
-- elaborateProg i ctx sCtx vCtx prog [] = 
--    error $ "\nProg: " ++ show prog ++ "\nContext: " ++ show ctx ++ "\nSContext: " ++ show sCtx ++ "\nVContext: " ++ show vCtx
elaborateProg i ctx sCtx vCtx prog []                  = Right (reverse prog, ctx, sCtx, vCtx)
elaborateProg i ctx sCtx vCtx prog (Assign x e : rest) = do
   let e' = getVars vCtx e 
   ty <- infer ctx e'
   let declare = Declare ty [] (Just e')
   let assign  = Assign [] e'
   elabAssign i ctx sCtx vCtx prog (Assign x e' : rest) ty declare assign  -- X could be undefined, reassigned, or redefined
elaborateProg i ctx sCtx vCtx prog (Block s : rest)    = do 
   (s', ctx', sCtx', vCtx') <- elaborateProg i ctx sCtx vCtx [] s  
   elaborateProg i ctx' sCtx' vCtx' (Block s' : prog) rest
elaborateProg i ctx sCtx vCtx prog (IfThen e (Block s) : rest)    = do 
   let e' = getVars vCtx e
   (s', ctx', sCtx', vCtx') <- elaborateProg i ctx sCtx vCtx [] s  
   elaborateProg i ctx' sCtx' vCtx' (IfThen e' (Block s') : prog) rest
elaborateProg i ctx sCtx vCtx prog (IfElse e (Block s) o : rest)    = do 
   let e' = getVars vCtx e
   (s, ctx, sCtx, vCtx) <- elaborateProg i ctx sCtx vCtx [] s  
   (s', ctx', sCtx', vCtx') <- elaborateProg i ctx sCtx vCtx [] [o]
   case s' of 
      [x] -> elaborateProg i ctx' sCtx' vCtx' (IfElse e' (Block s) x : prog) rest
      _   -> error "oops in elaborateProg"
elaborateProg i ctx sCtx vCtx prog (While e (Block s) : rest) = do 
   let e' = getVars vCtx e
   (s,ctx,sCtx,vCtx) <- elaborateProg i ctx sCtx vCtx [] s  
   elaborateProg i ctx sCtx vCtx (getExpr vCtx (While e' (Block s)) : prog) rest
elaborateProg i ctx sCtx vCtx prog (stmt : rest)       =
   elaborateProg i ctx sCtx vCtx (getExpr vCtx stmt : prog) rest

elabAssign :: Int -> Ctx -> SCtx -> VCtx -> Prog -> Prog -> Type -> Stmt -> Stmt -> Either TypeError (Prog, Ctx, SCtx, VCtx)
elabAssign i ctx sCtx vCtx nProg (Assign [] e : rest)     _  (Declare tyD eD e') (Assign eA e'') 
   | null eD && null eA = elaborateProg i ctx sCtx vCtx nProg rest
   | null eD            = elaborateProg i ctx sCtx vCtx (Assign (reverse eA) e'' : nProg) rest 
   | null eA            = elaborateProg i ctx sCtx vCtx (Declare tyD (reverse eD) e' : nProg) rest
   | otherwise = elaborateProg i ctx sCtx vCtx (Declare tyD (reverse eD) e' : Assign (reverse eA) e'' : nProg) rest
elabAssign i ctx sCtx vCtx nProg (Assign (x:xs) e : rest) ty da a = do 
   case M.lookup x ctx of
         -- Assignment before declaration is fine in Python, but need to add the declaration for explicit languages
      Nothing -> elabAssign i (M.insert x ty ctx) sCtx vCtx nProg (Assign xs e : rest) ty (addVar da x) a
      Just y  -> elabAssign' i ctx sCtx vCtx nProg (Assign (x:xs) e : rest) ty y da a
elabAssign i ctx sCtx vCtx nProg other _ _ _ = elaborateProg i ctx sCtx vCtx nProg other

elabAssign' :: Int -> Ctx -> SCtx -> VCtx -> Prog -> Prog -> Type -> Type -> Stmt -> Stmt -> Either TypeError (Prog, Ctx, SCtx, VCtx)
elabAssign' i ctx sCtx vCtx nProg (Assign (x:xs) e : rest) ty y da a
  | x `elem` M.elems vCtx = 
      let (x', i') = genVar i ctx
      in elabAssign i (M.insert x' ty ctx) sCtx (M.insert x x' vCtx) nProg (Assign xs e : rest) ty (addVar da x') a
  | y == ty               = elabAssign i ctx sCtx (M.delete x vCtx) nProg (Assign xs e : rest) ty da (addVar a x)
  | otherwise             = 
      case M.lookup x sCtx of
         Just vList ->
            case testSubVars ctx ty vList of
               Just z  -> elabAssign i ctx sCtx (M.insert x z vCtx) nProg (Assign xs e : rest) ty da (addVar a z)
               Nothing ->
                  let (x', i') = genVar i ctx
                  in elabAssign i' (M.insert x' ty ctx) (M.insert x (x' : vList) sCtx) (M.insert x x' vCtx) nProg (Assign xs e : rest) ty (addVar da x') a
         Nothing    ->
            let (x', i') = genVar i ctx
            in elabAssign i' (M.insert x' ty ctx) (M.insert x [x'] sCtx) (M.insert x x' vCtx) nProg (Assign xs e : rest) ty (addVar da x') a
elabAssign' _ _ _ _ _ _ _ _ _ _ = undefined

addVar :: Stmt -> Ident -> Stmt 
addVar (Declare ty x (Just e)) var = Declare ty (var : x) (Just e)
addVar (Assign x e)            var = Assign (var : x) e 
addVar rest                    _   = error ("Unexpected statement in addVar: " ++ show rest)

testSubVars :: Ctx -> Type -> [Ident] -> Maybe Ident
testSubVars ctx ty []     = Nothing
testSubVars ctx ty (x:xs) =
   case M.lookup x ctx of
      Nothing -> error "There will always be something here"
      Just y  ->
         if y == ty
         then Just x
         else testSubVars ctx ty xs


-- Type inference:
---------------------------------------------------------------------

infer :: Ctx -> Expr -> Either TypeError Type
infer _ (Lit (Int n))   = Right TyInt
infer _ (Lit (Bool b))  = Right TyBool
infer _ (Lit (Char c))  = Right TyChar
-- infer _ (Lit (Str [s])) = Right TyChar
infer _ (Lit (Str s))   = Right TyStr
infer ctx (Var v) =
   case M.lookup v ctx of
      Nothing -> Left (UndefinedVar v)
      Just x  -> Right x
infer ctx (Bin op e1 e2) = do
   e1' <- infer ctx e1
   check ctx e2 e1' *>
      case op of
         And -> check ctx e1 TyBool *> Right TyBool
         Or  -> check ctx e1 TyBool *> Right TyBool
         Eq  -> Right TyBool
         NEq -> Right TyBool
         Less -> Right TyBool
         Greater -> Right TyBool
         LessEq -> Right TyBool
         GreaterEq -> Right TyBool   
         -- Mul -> Can be num -> num -> num or string -> num -> [string?] regardless, need to allow for two different types      
         Add ->
            if e1' == TyInt
            then Right TyInt
            else
               if e1' == TyStr
               then Right TyStr
               else do 
                  e2' <- infer ctx e2 
                  Left (TypeMismatch e1' e2')
         x   -> check ctx e1 TyInt *> Right TyInt
infer ctx (Un op e) =
   case op of
      Neg -> check ctx e TyInt *> Right TyInt
      Not -> check ctx e TyBool *> Right TyBool
infer ctx (Array [])       = Right (TyArr Poly)  -- Type constraints needed?
infer ctx (Array (x:rest)) = do 
   xTy <- infer ctx x
   checkArray ctx xTy rest
infer ctx x    = error ("The following expression is not yet implemented: " ++ show x) -- so this should show the actual expression

checkArray :: Ctx -> Type -> [Expr] -> Either TypeError Type
checkArray _   ty []       = Right (TyArr ty)
checkArray ctx ty (x:rest) = check ctx x ty *> checkArray ctx ty rest

check :: Ctx -> Expr -> Type -> Either TypeError ()
check ctx e ty = do
   ty' <- infer ctx e
   if ty == ty'
   then Right ()
   else Left (TypeMismatch ty ty')

