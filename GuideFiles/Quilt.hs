


{-# OPTIONS_GHC -Wall #-}


{-# LANGUAGE GADTs #-}


module Quilt where

import Parsing2
import Text.Parsec.Token (GenTokenParser(naturalOrFloat))


import qualified Data.Map as M


-- | A color is a list of red, green, and blue values between 0.0 - 1.0.
--   For example, [0,0,0] is black, [1,1,1] is white, [0.5, 0, 0.5] is a
--   darkish purple, and so on.
type Color = [Double]

-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].

type QuiltFun = Double -> Double -> Color

data ColorLit where
    Red    :: ColorLit
    Blue   :: ColorLit
    Green  :: ColorLit
    Yellow :: ColorLit
    Purple :: ColorLit
    Pink   :: ColorLit
    Orange :: ColorLit
    Grey   :: ColorLit
    White  :: ColorLit
    Black  :: ColorLit
    deriving (Show, Eq)

data Arith where
    Plus  :: Arith
    Minus :: Arith
    Times :: Arith
    Div   :: Arith
    Exp   :: Arith
    deriving (Show, Eq)

data Comparison where
    LessThan     :: Comparison
    GreaterThan  :: Comparison
    LessEqual    :: Comparison
    GreaterEqual :: Comparison
    Equals       :: Comparison
    NotEquals    :: Comparison
    deriving (Show, Eq)

data BoolOp where
    And :: BoolOp
    Or  :: BoolOp
    deriving (Show, Eq)

data BOp where
    BArith  :: Arith -> BOp
    BComp   :: Comparison -> BOp
    BBoolOp :: BoolOp -> BOp
    deriving (Show)

data UOp where
    Neg  :: UOp
    Not  :: UOp
    Sin  :: UOp
    Cos  :: UOp
    Tan  :: UOp
    ExpE :: UOp
    Abs  :: UOp
    Sqrt :: UOp
    deriving (Show, Eq)

data Coord where
    X :: Coord
    Y :: Coord
    deriving Show

{- Initially defined `ERgb` as `[QExpr] -> QExpr`, but
was having difficulties figuring out how to parse that
Thinking I may need to change `ERgb` to `QExpr -> QExpr -> QExpr -> QExpr`
and then have to add a `brackets` import parsec
and parse a QExpr until I reach a comma, and repeat.
-}

data QTranscendental where
    TPi :: QTranscendental
    TE  :: QTranscendental
    deriving (Show, Eq)

data QExpr where
    EColor   :: ColorLit -> QExpr
    ENumber  :: Either Integer Double -> QExpr
    TChar    :: QTranscendental -> QExpr
    EBool    :: Bool -> QExpr
    ERgb     :: QExpr -> QExpr -> QExpr -> QExpr
    If       :: QExpr -> QExpr -> QExpr -> QExpr
    Un       :: UOp -> QExpr -> QExpr
    Bin      :: BOp -> QExpr -> QExpr -> QExpr
    EQuilt   :: QExpr -> QExpr -> QExpr -> QExpr -> QExpr
    Location :: Coord -> QExpr
    deriving Show

type Ctx = M.Map String Type

data Error where
    TypeError      :: QExpr -> Type -> Type -> Error
    DivByZero      :: Error
    QuiltTypeError :: QExpr -> Error
    deriving (Show)

data Type where 
    TColor  :: Type
    TBool   :: Type
    TNumber :: Type
    TNumOrC :: Type  -- For when pretty-printing arithmetic expressions.
    deriving (Show, Eq)

-- Parsing 
------------------------------------------------------------------------------------------------------------------------------

lexer :: TokenParser u
lexer = makeTokenParser $ emptyDef
  { reservedNames = [ "red", "blue", "green", "yellow", "orange", "pink"
                    , "purple", "grey", "gray", "white", "black", "if"
                    , "then", "else","False", "True", "quilt", "x", "y"
                    , "sin", "cos", "tan", "exp", "sqrt", "abs", "pi", "e" ] }

parens :: Parser a -> Parser a
parens = getParens lexer

brackets :: Parser a -> Parser a
brackets = getBrackets lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

integer :: Parser Integer
integer = getInteger lexer

double :: Parser Double 
double = getFloat lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

identifier :: Parser String
identifier = getIdentifier lexer

parseEQuilt :: Parser QExpr
parseEQuilt = EQuilt 
    <$ reserved "quilt" 
    <*> parseQExpr 
    <*> parseQExpr 
    <*> parseQExpr 
    <*> parseQExpr

parseENumber :: Parser QExpr
parseENumber = ENumber <$> naturalOrFloat lexer

parseTChar :: Parser QExpr
parseTChar = TChar TPi <$ reserved "pi" <|> TChar TE <$ reserved "e"

parseERgb :: Parser QExpr
parseERgb = ERgb 
    <$> (parseQExpr <* reservedOp ",") 
    <*> (parseQExpr <* reservedOp ",") 
    <*> parseQExpr

parseEBool :: Parser QExpr 
parseEBool = EBool True <$ reserved "True" <|> EBool False <$ reserved "False"

parseEColor :: Parser QExpr
parseEColor = EColor Red <$ reserved "red"
    <|> EColor Blue   <$ reserved "blue"
    <|> EColor Green  <$ reserved "green"
    <|> EColor Pink   <$ reserved "pink"
    <|> EColor Purple <$ reserved "purple"
    <|> EColor Orange <$ reserved "orange"
    <|> EColor Yellow <$ reserved "yellow"
    <|> EColor White  <$ reserved "white"
    <|> EColor Black  <$ reserved "black"
    <|> (EColor Grey  <$ reserved "grey" <|> EColor Grey <$ reserved "gray")

parseIf :: Parser QExpr
parseIf = If
  <$> (reserved "if"   *> parseQExpr)
  <*> (reserved "then" *> parseQExpr)
  <*> (reserved "else" *> parseQExpr)

parseCoord :: Parser QExpr
parseCoord = Location X <$ reserved "x" <|> Location Y <$ reserved "y"

parseQExprAtom :: Parser QExpr
parseQExprAtom = parens parseQExpr 
    <|> parseEColor 
    <|> parseENumber 
    <|> parseTChar
    <|> parseIf 
    <|> parseEBool 
    <|> parseEQuilt 
    <|> brackets parseERgb
    <|> parseCoord

{- The following two trig examples are parsing correctly but are returning 
slightly different images than the ones displayed on the project page.
    (-cos(8*pi*x)/2 + 0.5) * green
    -cos(50*sqrt(x*x + y*y))/4 + -cos(50*(sqrt((x-0.2)*(x-0.2) + y*y)))/4 + 0.5
I assume my precedence is slightly incorrect and is causing the variations.

After moving the trig functions to the top of the chart (as was 
explicitly hinted at on the project page), the problem was fixed.
-}

parseQExpr :: Parser QExpr
parseQExpr = buildExpressionParser table parseQExprAtom
  where
    table = [   [ Prefix (Un Abs  <$ reserved "abs")
                , Prefix (Un Sin  <$ reserved "sin")
                , Prefix (Un Cos  <$ reserved "cos")
                , Prefix (Un Tan  <$ reserved "tan")
                , Prefix (Un ExpE <$ reserved "exp")
                ]
            ,   [ Prefix (Un Neg <$ reservedOp "-")
                , Prefix (Un Not <$ reservedOp "!")
                ]
            ,   [ Infix (Bin (BArith Exp) <$ reservedOp "^") AssocRight
                , Prefix (Un Sqrt <$ reserved "sqrt")
                ]
            ,   [ Infix (Bin (BArith Times) <$ reservedOp "*") AssocLeft
                , Infix (Bin (BArith Div) <$ reservedOp "/") AssocLeft
                ]
            ,   [ Infix (Bin (BArith Plus) <$ reservedOp "+") AssocLeft
                , Infix (Bin (BArith Minus) <$ reservedOp "-") AssocLeft
                ]
            ,   [ Infix (Bin (BComp LessThan) <$ reservedOp "<") AssocNone
                , Infix (Bin (BComp GreaterThan) <$ reservedOp ">") AssocNone
                , Infix (Bin (BComp LessEqual) <$ reservedOp "<=") AssocNone
                , Infix (Bin (BComp GreaterEqual) <$ reservedOp ">=") AssocNone
                ]
            ,   [ Infix (Bin (BComp NotEquals) <$ reservedOp "!=") AssocNone
                , Infix (Bin (BComp Equals) <$ reservedOp "==") AssocNone
                ]
            ,   [ Infix (Bin (BBoolOp And) <$ reservedOp "&&") AssocRight
                ]
            ,   [ Infix (Bin (BBoolOp Or) <$ reservedOp "||") AssocRight
                ]
            ]

quilt :: Parser QExpr
quilt = whiteSpace *> parseQExpr <* eof

-- Pretty-Printing
------------------------------------------------------------------------------------------------------------------------------

prettyPrint :: QExpr -> String 
prettyPrint (EColor color)       = case color of 
    Red    -> "red"
    Blue   -> "blue"
    Green  -> "green"
    Yellow -> "yellow"
    Purple -> "purple"
    Pink   -> "pink"
    Orange -> "orange"
    Grey   -> "grey"
    White  -> "white"
    Black  -> "black"
prettyPrint (ENumber (Left i))   = prettyPrint (ENumber (Right (fromIntegral i)))
prettyPrint (ENumber (Right i))  = show i
prettyPrint (TChar c)            = case c of 
    TPi -> "pi"
    TE  -> "e" 
prettyPrint (ERgb r g b)         = 
    "[" ++ prettyPrint r ++ ", " 
    ++ prettyPrint g ++ ", " 
    ++ prettyPrint b ++ "]"
prettyPrint (EBool b)            = 
    if b 
    then "True" 
    else "False"
prettyPrint (If e1 e2 e3)        = "if " ++ prettyPrint e1 
    ++ " then " ++ prettyPrint e2 
    ++ " else " ++ prettyPrint e3 ++ "."
prettyPrint (Un uOp e1)          = case uOp of 
    Neg  -> "-" ++ prettyPrint e1
    Not  -> "!" ++ prettyPrint e1
    Sin  -> "sin("  ++ prettyPrint e1 ++ ")"
    Cos  -> "cos("  ++ prettyPrint e1 ++ ")"
    Tan  -> "tan("  ++ prettyPrint e1 ++ ")"
    ExpE -> "exp("  ++ prettyPrint e1 ++ ")"
    Abs  -> "abs("  ++ prettyPrint e1 ++ ")"
    Sqrt -> "sqrt(" ++ prettyPrint e1 ++ ")"
prettyPrint (Bin op e1 e2)       = prettyPrint e1 ++ prettyBOp op ++ prettyPrint e2
prettyPrint (EQuilt e1 e2 e3 e4) = "quilt " 
    ++ prettyPrint e1 ++ " "
    ++ prettyPrint e2 ++ " "
    ++ prettyPrint e3 ++ " "
    ++ prettyPrint e4
prettyPrint (Location X)         = "x"
prettyPrint (Location Y)         = "y"

prettyBOp :: BOp -> String 
prettyBOp (BArith arith)   = case arith of 
    Plus  -> " + "
    Minus -> " - "
    Times -> " * "
    Div   -> " / "
    Exp   -> " ^ "
prettyBOp (BComp comp)     = case comp of 
    LessThan     -> " < "
    GreaterThan  -> " > "
    LessEqual    -> " <= "
    GreaterEqual -> " >= "
    Equals       -> " == "
    NotEquals    -> " /= "
prettyBOp (BBoolOp boolOp) = case boolOp of 
    And -> " && "
    Or  -> " || "

showError :: Error -> String
showError (TypeError e t1 t2)  = 
    "Type error: the expression `" ++ prettyPrint e 
    ++ "` was expected to be a " ++ printType t1 
    ++ ", but is actually a " ++ printType t2 ++ "."
showError DivByZero            = "Division error: division by 0."
showError (QuiltTypeError _)   = "Quilt type error: the types of a quilt must either "
    ++ "be all booleans or a mixture of numbers and colors. "

printType :: Type -> String
printType TBool   = "bool"
printType TNumber = "number"
printType TColor  = "color"
printType TNumOrC = "number or color"

-- Type checking
------------------------------------------------------------------------------------------------------------------------------

isSubtype :: Type -> Type -> Bool
isSubtype TBool TBool     = True
isSubtype TNumber TNumber = True
isSubtype TColor TColor   = True
isSubtype TNumber TColor  = True
isSubtype TColor TNumber  = True
isSubtype _ _             = False

getSubtype :: Type -> Type -> Type
getSubtype TBool TBool     = TBool
getSubtype TNumber TNumber = TNumber
getSubtype TColor TColor   = TColor
getSubtype TNumber TColor  = TColor
getSubtype TColor TNumber  = TColor
getSubtype _ _             = undefined

validQuiltSubtype :: Type -> Type -> Type -> Type -> Bool
validQuiltSubtype TBool TBool TBool TBool = True
validQuiltSubtype TBool _ _ _             = False
validQuiltSubtype _ TBool _ _             = False
validQuiltSubtype _ _ TBool _             = False
validQuiltSubtype _ _ _ TBool             = False
validQuiltSubtype _ _ _ _                 = True

getQuiltSubtype :: Type -> Type -> Type -> Type -> Type
getQuiltSubtype TBool TBool TBool TBool         = TBool
getQuiltSubtype TNumber TNumber TNumber TNumber = TNumber
getQuiltSubtype _ _ _ _                         = TColor

infer :: Ctx -> QExpr -> Either Error Type
infer _ (EColor _)                = Right TColor
infer _ (ENumber _)               = Right TNumber
infer _ (EBool _)                 = Right TBool
infer _ (Location _)              = Right TNumber
infer _ (TChar _)                 = Right TNumber
infer ctx (ERgb r g b)            = check ctx r TNumber 
    *> check ctx g TNumber 
    *> check ctx b TNumber 
    *> Right TColor
infer ctx (If e1 e2 e3)           = check ctx e1 TBool 
    *> infer ctx e2 >>= \e2' -> 
    infer ctx e3 >>= \e3' ->
        if isSubtype e2' e3' 
        then Right (getSubtype e2' e3') 
        else Left (TypeError (If e1 e2 e3) e2' e3')
infer ctx (Bin (BArith _) e1 e2)  = infer ctx e1 >>= \e1' -> 
    infer ctx e2 >>= \e2' ->
        if e1' /= TBool
        then
            if e2' /= TBool 
            then Right (getSubtype e1' e2')
            else Left (TypeError e2 TNumOrC e2')
        else Left (TypeError e1 TNumOrC e1')
infer ctx (Bin (BBoolOp _) e1 e2) = check ctx e1 TBool 
    *> check ctx e2 TBool 
    *> Right TBool
infer ctx (Bin (BComp _) e1 e2)   = check ctx e1 TNumber 
    *> check ctx e2 TNumber 
    *> Right TBool
infer ctx (Un x e1)               = case x of 
    Not -> check ctx e1 TBool   *> Right TBool
    _   -> check ctx e1 TNumber *> Right TNumber
infer ctx (EQuilt e1 e2 e3 e4)    = infer ctx e1 >>= \e1' ->
    infer ctx e2 >>= \e2' ->
    infer ctx e3 >>= \e3' ->
    infer ctx e4 >>= \e4' ->
        if validQuiltSubtype e1' e2' e3' e4' 
        then Right (getQuiltSubtype e1' e2' e3' e4') 
        else Left (QuiltTypeError (EQuilt e1 e2 e3 e4))

check :: Ctx -> QExpr -> Type -> Either Error ()
check context e t = 
    case infer context e of
        Left x  -> Left x
        Right x -> 
            if t == x 
            then Right () 
            else Left (TypeError e t x)

inferQExpr :: QExpr -> Either Error Type 
inferQExpr = infer M.empty

-- Interpreter
------------------------------------------------------------------------------------------------------------------------------

interpQExpr :: QExpr -> Either Error QuiltFun
interpQExpr (Location X)         = Right $ \x _ -> [x, x, x]
interpQExpr (Location Y)         = Right $ \_ y -> [y, y, y]
interpQExpr (EColor n)           = Right $ \_ _ -> 
    case n of
        Red    -> [1, 0, 0]
        Green  -> [0, 1, 0]
        Blue   -> [0, 0, 1]
        Yellow -> [1, 1, 0]
        Pink   -> [1, 0, 1]
        Purple -> [0.627, 0.125, 0.941]
        Orange -> [1, 0.647, 0]
        Grey   -> [0.5, 0.5, 0.5]
        White  -> [1, 1, 1]
        Black  -> [0, 0, 0]
interpQExpr (TChar TPi)          = Right $ \_ _ -> [pi, pi, pi]
interpQExpr (TChar TE)           = Right $ \_ _ -> [exp 1, exp 1, exp 1]
interpQExpr (ENumber n)          = Right $ \_ _ -> [numbToDouble n, numbToDouble n, numbToDouble n]
interpQExpr (ERgb r g b)         = interpQExpr r >>= \r' -> 
    interpQExpr g >>= \g' -> 
    interpQExpr b >>= \b' ->
        Right $ \x y -> 
            let rValue = r' x y 
            in let gValue = g' x y 
            in let bValue = b' x y 
            in [eRgbHelp rValue, eRgbHelp gValue, eRgbHelp bValue]
interpQExpr (EBool bool)         = Right $ \_ _ -> 
    if bool 
    then [1, 1, 1] 
    else [0, 0, 0]
interpQExpr (If e1 e2 e3)        = interpQExpr e1 >>= \e1' -> 
    interpQExpr e2 >>= \e2' ->
    interpQExpr e3 >>= \e3' ->
        Right $ \x y ->
            let e1Color = e1' x y 
            in if e1Color == [1, 1, 1] 
               then e2' x y 
               else e3' x y
interpQExpr (Bin op e1 e2)       = interpQExpr e1 >>= \e1' -> 
    interpQExpr e2 >>= \e2' ->
        case op of
            BArith arith  -> arithHelper arith e1' e2'
            BComp comp    -> compHelper comp e1' e2'
            BBoolOp andOr -> boolHelper andOr e1' e2'
interpQExpr (Un op e1)           = interpQExpr e1 >>= \e1' -> unHelper op e1'
interpQExpr (EQuilt e1 e2 e3 e4) = interpQExpr e1 >>= \e1' ->
    interpQExpr e2 >>= \e2' ->
    interpQExpr e3 >>= \e3' ->
    interpQExpr e4 >>= \e4' ->
        Right $ \x y -> case (x < 0, y > 0) of
            (True, True)  -> e1' (x*2 + 1) (y*2 - 1)
            (False,True)  -> e2' (x*2 - 1) (y*2 - 1)
            (True, False) -> e3' (x*2 + 1) (y*2 + 1)
            (False,False) -> e4' (x*2 - 1) (y*2 + 1)

-- I wrote several helper functions in an attempt to make the interpreter less overwhelming.

arithHelper :: Arith -> QuiltFun -> QuiltFun -> Either Error QuiltFun
arithHelper op e1 e2 = Right $ \x y ->
    case e1 x y of
        [r1, g1, b1] -> case e2 x y of
            [r2, g2, b2] -> case op of
                Plus    -> [r1 + r2, g1 + g2, b1 + b2]
                Minus   -> [r1 - r2, g1 - g2, b1 - b2]
                Times   -> [r1 * r2, g1 * g2, b1 * b2]
                Div     -> [r1 / r2, g1 / g2, b1 / b2]
                Exp     -> [r1 ** r2, g1 ** g2, b1 ** b2]
            _            -> undefined
        _         -> undefined
        
{- I had a very difficult time figuring out how to access the `Color` within a `QuiltFun`.
After trying everything else that I could think of, I decided to experiment 
with a let expression for the first time and found success.
My initial implementations looked like the following: 
    `let [r, g, b] = e1 x y 
    in case uOp of 
        ...`
This worked, but I changed it due to a non-exhaustive pattern matching warning.
-}

compHelper :: Comparison -> QuiltFun -> QuiltFun -> Either Error QuiltFun
compHelper comp e1 e2 = Right $ \x y ->
    let e1' = e1 x y 
    in let e2' = e2 x y 
    in case comp of
        LessThan     -> 
            if e1' < e2' 
            then [1, 1, 1] 
            else [0, 0, 0]
        GreaterThan  -> 
            if e1' > e2' 
            then [1, 1, 1] 
            else [0, 0, 0]
        LessEqual    -> 
            if e1' < e2' || e1' == e2' 
            then [1, 1, 1] 
            else [0, 0, 0]
        GreaterEqual -> 
            if e1' > e2' || e1' == e2' 
            then [1, 1, 1] 
            else [0, 0, 0]
        Equals       -> 
            if e1' == e2' 
            then [1, 1, 1] 
            else [0, 0, 0]
        NotEquals    -> 
            if e1' /= e2' 
            then [1, 1, 1] 
            else [0, 0, 0]

boolHelper :: BoolOp -> QuiltFun -> QuiltFun -> Either Error QuiltFun
boolHelper bOp e1 e2 = Right $ \x y ->
    let e1' = e1 x y 
    in let e2' = e2 x y 
    in case bOp of 
        And -> 
            if e1' == [1, 1, 1] && e2' == [1, 1, 1] 
            then [1, 1, 1] 
            else [0, 0, 0]
        Or  -> 
            if e1' == [1, 1, 1] || e2' == [1, 1, 1] 
            then [1, 1, 1] 
            else [0, 0, 0]

unHelper :: UOp -> QuiltFun -> Either Error QuiltFun
unHelper uOp e1 = Right $ \x y ->
    case e1 x y of 
        [r, g, b] ->  case uOp of 
            Neg  -> [r * (-1), g * (-1), b * (-1)]
            Sin  -> [sin r, sin g, sin b]
            Cos  -> [cos r, cos g, cos b]
            Tan  -> [tan r, tan g, tan b]
            ExpE -> [exp r, exp g, exp b]
            Abs  -> [abs r, abs g, abs b]
            Sqrt -> [sqrt r, sqrt g, sqrt b]
            Not  -> 
                if [r, g, b] == [1, 1, 1]
                then [0, 0, 0] 
                else [1, 1, 1]
        _         -> undefined

numbToDouble :: Either Integer Double -> Double
numbToDouble (Left i)  = fromIntegral i
numbToDouble (Right i) = i

eRgbHelp :: [Double] -> Double
eRgbHelp [c1, _, _] = c1
eRgbHelp _          = 0.0

evalQuilt :: String -> Either String QuiltFun
evalQuilt s = case parse quilt s of
    Left err -> Left (show err)
    Right x  -> 
        case inferQExpr x of 
            Left y  -> Left (showError y)
            Right _ -> 
                case interpQExpr x of
                    Left z  -> Left (showError z)
                    Right z -> Right z
