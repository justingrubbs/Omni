-- Final Project
-- Justin Grubbs


-- https://www.alt-codes.net/music_note_alt_codes.php
-- https://www.libertyparkmusic.com/musical-time-signatures/
-- https://jadebultitude.com/chords-in-c-major/
-- https://musictheory.pugetsound.edu/mt21c/MajorKeySignatures.html
-- https://www.skoove.com/blog/sight-reading-ties-explained/#:~:text=Music%20ties%20usually%20only%20tie,are%20always%20the%20same%20pitch.
-- https://www.savannahmusicfestival.org/wp-content/uploads/2020/04/Ties-and-Slurs-Worksheet.pdf


{-# OPTIONS_GHC -Wall #-}


{-# LANGUAGE GADTs #-}

--  VSCode hint to avoid using an unfamiliar expression in exprToString
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}  
{-# HLINT ignore "Use foldl" #-}


module MComp where

import           Parsing2

import qualified Data.Map           as M


{-  I debated on how I wanted my project to function for a long time. 
    I thought that having the user typing in expressions in the terminal 
    (as in previous projects) would be too tedious for this project. I 
    decided to model my project after Module 11 - IMP, so that the user 
    can construct an entire composition rather than a single expression.  -}


type Var   = String

type Prog  = [Stmt]

type Group = [Expr]

data Type where
    TyNote  :: Type
    TyMeas  :: Type
    TyComm  :: Type
    deriving (Show, Eq)


{-  I am not sure that my project requires distinguishing between statements and 
    expressions, as my project does not involve conditionals or evaluations.  -}

{-  After beginning to implement type checking, I realize the benefits of 
    distinguishing between statements and expressions. I think it could
    function without the distinction, but I think it would make the project
    easier for me to rationalize and breakdown.  -}

{-  After completing parsing and type-checking, it has occurred to me that implementing
    ties as a binary operator would probably be cleaner and more effective. Instead of
    typing `tieN (q c, q c);` and `tieM ([measure]; [measure];);`, 
    it could just be `q c -> q c;` and `[measure] -> [measure];`.
    This would also help with outputting tied notes, as I have not found
    a clean way to do so otherwise.  

    In creating a binary operator, it occurred to me that I could represent the other
    commands as unary operators. I would do this, but I can't think of a good
    symbol that would appropriately represent chords, triads, or sevenths.  -}


data Stmt where
    Decl    :: Type -> Var -> Stmt
    Assign  :: Var -> Expr -> Stmt
    StExpr  :: Expr -> Stmt
    deriving Show

data EChord where  -- A group of notes to be played at the same time (Custom can be MNotes)
    Custom  :: Group -> EChord  
    Triad   :: ETriad -> EChord  
    Seventh :: ESeventh -> EChord
    deriving Show

data Expr where
    Var     :: Var -> Expr
    Key     :: Scale -> Expr
    TimeSig :: Integer -> Integer -> Expr
    Note    :: Rhythm -> Pitch -> Expr
    MNotes  :: Group -> Expr  -- A group of notes to be played sequentially
    Meas    :: Group -> Expr
    Block   :: Group -> Expr
    Chord   :: EChord -> Expr
    Bin     :: Op -> Expr -> Expr -> Expr
    deriving Show

data Op where 
    Tie :: Op 
    deriving (Show, Eq)

data ETriad where
    T1 :: Rhythm -> ETriad
    T2 :: Rhythm -> ETriad
    T3 :: Rhythm -> ETriad
    T4 :: Rhythm -> ETriad
    T5 :: Rhythm -> ETriad
    T6 :: Rhythm -> ETriad
    T7 :: Rhythm -> ETriad
    deriving Show

data ESeventh where
    S1 :: Rhythm -> ESeventh
    S2 :: Rhythm -> ESeventh
    S3 :: Rhythm -> ESeventh
    S4 :: Rhythm -> ESeventh
    S5 :: Rhythm -> ESeventh
    S6 :: Rhythm -> ESeventh
    S7 :: Rhythm -> ESeventh
    deriving Show

data Scale where
-- Major Scales:
    CMajor  :: Scale
    GMajor  :: Scale
    DMajor  :: Scale
    AMajor  :: Scale
    EMajor  :: Scale
    FMajor  :: Scale
    BFlat   :: Scale
    EFlat   :: Scale
    AFlat   :: Scale
    BMajor  :: Scale
    CFlat   :: Scale
    FSharp  :: Scale
    GFlat   :: Scale
    CSharp  :: Scale
    DFlat   :: Scale
-- Minor Scales:
    AMinor  :: Scale
    EMinor  :: Scale
    BMinor  :: Scale
    FSMinor :: Scale
    CSMinor :: Scale
    GSMinor :: Scale
    DSMinor :: Scale
    ASMinor :: Scale
    AFMinor :: Scale
    EFMinor :: Scale
    BFMinor :: Scale
    FMinor  :: Scale
    CMinor  :: Scale
    GMinor  :: Scale
    DMinor  :: Scale
    deriving Show

data Rhythm where
    ThirtySec :: Rhythm
    Sixteenth :: Rhythm
    Eighth    :: Rhythm
    Quarter   :: Rhythm
    Half      :: Rhythm
    Whole     :: Rhythm
    TSDot     :: Rhythm
    SDot      :: Rhythm
    EDot      :: Rhythm
    QDot      :: Rhythm
    HDot      :: Rhythm
    WDot      :: Rhythm
    TSDoubDot :: Rhythm
    SDoubDot  :: Rhythm
    EDoubDot  :: Rhythm
    QDoubDot  :: Rhythm
    HDoubDot  :: Rhythm
    WDoubDot  :: Rhythm
    deriving (Show, Eq)

data Pitch where 
    A    :: Maybe Accidental -> Pitch
    B    :: Maybe Accidental -> Pitch
    C    :: Maybe Accidental -> Pitch
    D    :: Maybe Accidental -> Pitch
    E    :: Maybe Accidental -> Pitch
    F    :: Maybe Accidental -> Pitch
    G    :: Maybe Accidental -> Pitch
    Rest :: Pitch
    deriving (Show, Eq)

data Accidental where
    Sharp     :: Accidental
    Flat      :: Accidental
    Nat       :: Accidental
    DoubSharp :: Accidental
    DoubFlat  :: Accidental
    deriving (Show, Eq)


-- Parsing 
----------------------------------------------------------------------------------------


lexer :: TokenParser u
lexer = makeTokenParser $ emptyDef
  { reservedNames   =   ["testytest", "note", "measure", "block", "quarter", "q", "tri", "sev"
                        , "eighth", "e", "sixteenth", "s", "thirty-second", "ts"
                        , "half", "h", "whole", "w", "chord", "triad", "seventh", "a"
                        , "an", "ab", "abb", "b", "bn", "bb", "bbb", "c", "cn", "cb"
                        , "cbb", "d", "dn", "db", "dbb", "e", "en", "eb", "ebb", "f"
                        , "fn", "fb", "fbb", "g", "gn", "gb", "gbb", "A", "Ab", "B"
                        , "Bb", "C", "Cb", "D", "Db", "E", "Eb", "F", "G", "Gb"
                        , "rest", "r", "1", "2", "3", "4", "5", "6", "7" ] 
  , reservedOpNames =   ["->" ]
  }

parens :: Parser a -> Parser a
parens = getParens lexer

brackets :: Parser a -> Parser a
brackets = getBrackets lexer

symbol :: String -> Parser String
symbol = getSymbol lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

integer :: Parser Integer
integer = getInteger lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

identifier :: Parser String
identifier = getIdentifier lexer

parseRhythm :: Parser Rhythm
parseRhythm = QDoubDot <$ try (reserved "quarter" *> symbol ".." <|> reserved "q" *> symbol "..")
    <|> QDot      <$ try (reserved "quarter" *> symbol "." <|> reserved "q" *> symbol ".")
    <|> Quarter   <$ (reserved "quarter" <|> reserved "q")
    <|> EDoubDot  <$ try (reserved "eighth" *> symbol ".." <|> reserved "e" *> symbol "..")
    <|> EDot      <$ try (reserved "eighth" *> symbol "." <|> reserved "e" *> symbol ".")
    <|> Eighth    <$ (reserved "eighth" <|> reserved "e")
    <|> HDoubDot  <$ try (reserved "half" *> symbol ".." <|> reserved "h" *> symbol "..")
    <|> HDot      <$ try (reserved "half" *> symbol "." <|> reserved "h" *> symbol ".")
    <|> Half      <$ (reserved "half" <|> reserved "h")
    <|> SDoubDot  <$ try (reserved "sixteenth" *> symbol ".." <|> reserved "s" *> symbol "..")
    <|> SDot      <$ try (reserved "sixteenth" *> symbol "." <|> reserved "s" *> symbol ".")
    <|> Sixteenth <$ (reserved "sixteenth" <|> reserved "s")
    <|> WDoubDot  <$ try (reserved "whole" *> symbol ".." <|> reserved "w" *> symbol "..")
    <|> WDot      <$ try (reserved "whole" *> symbol "." <|> reserved "w" *> symbol ".")
    <|> Whole     <$ (reserved "whole" <|> reserved "w")
    <|> TSDoubDot <$ try (reserved "thirty-second" *> symbol ".." <|> reserved "ts" *> symbol "..")
    <|> TSDot     <$ try (reserved "thirty-second" *> symbol "." <|> reserved "ts" *> symbol ".")
    <|> ThirtySec <$ (reserved "thirty-second" <|> reserved "ts")

parsePitch :: Parser Pitch
parsePitch = parseA
    <|> parseB
    <|> parseC
    <|> parseD
    <|> parseE
    <|> parseF
    <|> parseG
    <|> Rest <$ (reserved "rest" <|> reserved "r")

parseA :: Parser Pitch
parseA = A (Just DoubSharp) <$ try (reserved "a" *> symbol "##")
    <|> A (Just Sharp)    <$ try (reserved "a" *> symbol "#")
    <|> A Nothing         <$ reserved "a"
    <|> A (Just DoubFlat) <$ reserved "abb"
    <|> A (Just Flat)     <$ reserved "ab"
    <|> A (Just Nat)      <$ reserved "an"

parseB :: Parser Pitch
parseB = B (Just DoubSharp) <$ try (reserved "b" *> symbol "##")
    <|> B (Just Sharp)    <$ try (reserved "b" *> symbol "#")
    <|> B Nothing         <$ reserved "b"
    <|> B (Just Flat)     <$ reserved "bb"
    <|> B (Just Nat)      <$ reserved "bn"
    <|> B (Just DoubFlat) <$ reserved "bbb"

parseC :: Parser Pitch
parseC = C (Just DoubSharp) <$ try (reserved "c" *> symbol "##")
    <|> C (Just Sharp)    <$ try (reserved "c" *> symbol "#")
    <|> C Nothing         <$ reserved "c"
    <|> C (Just Flat)     <$ reserved "cb"
    <|> C (Just Nat)      <$ reserved "cn"
    <|> C (Just DoubFlat) <$ reserved "cbb"

parseD :: Parser Pitch
parseD = D (Just DoubSharp) <$ try (reserved "d" *> symbol "##")
    <|> D (Just Sharp)    <$ try (reserved "d" *> symbol "#")
    <|> D Nothing         <$ reserved "d"
    <|> D (Just Flat)     <$ reserved "db"
    <|> D (Just Nat)      <$ reserved "dn"
    <|> D (Just DoubFlat) <$ reserved "dbb"

parseE :: Parser Pitch
parseE = E (Just DoubSharp) <$ try (reserved "e" *> symbol "##")
    <|> E (Just Sharp)    <$ try (reserved "e" *> symbol "#")
    <|> E Nothing         <$ reserved "e"
    <|> E (Just Flat)     <$ reserved "eb"
    <|> E (Just Nat)      <$ reserved "en"
    <|> E (Just DoubFlat) <$ reserved "ebb"

parseF :: Parser Pitch
parseF = F (Just DoubSharp) <$ try (reserved "f" *> symbol "##")
    <|> F (Just Sharp)    <$ try (reserved "f" *> symbol "#")
    <|> F Nothing         <$ reserved "f"
    <|> F (Just Flat)     <$ reserved "fb"
    <|> F (Just Nat)      <$ reserved "fn"
    <|> F (Just DoubFlat) <$ reserved "fbb"

parseG :: Parser Pitch
parseG = G (Just DoubSharp) <$ try (reserved "g" *> symbol "##")
    <|> G (Just Sharp)    <$ try (reserved "g" *> symbol "#")
    <|> G Nothing         <$ reserved "g"
    <|> G (Just Flat)     <$ reserved "gb"
    <|> G (Just Nat)      <$ reserved "gn"
    <|> G (Just DoubFlat) <$ reserved "gbb"

parseGroupC :: Parser Group
parseGroupC = parseExpr `sepBy` reservedOp ","

parseGroupS :: Parser Group
parseGroupS = parseExpr `endBy` reservedOp ";"

parseScale :: Parser Scale
parseScale = CSharp <$ try (reserved "C" <* symbol "#")
    <|> FSharp  <$ try (reserved "F" <* symbol "#")
    <|> FSMinor <$ try (reserved "f" <* symbol "#")
    <|> CSMinor <$ try (reserved "c" <* symbol "#")
    <|> GSMinor <$ try (reserved "g" <* symbol "#")
    <|> DSMinor <$ try (reserved "d" <* symbol "#")
    <|> ASMinor <$ try (reserved "a" <* symbol "#")
    <|> CMajor  <$ reserved "C"
    <|> GMajor  <$ reserved "G"
    <|> DMajor  <$ reserved "D"
    <|> AMajor  <$ reserved "A"
    <|> EMajor  <$ reserved "E"
    <|> FMajor  <$ reserved "F"
    <|> BFlat   <$ reserved "Bb"
    <|> EFlat   <$ reserved "Eb"
    <|> AFlat   <$ reserved "Ab"
    <|> BMajor  <$ reserved "B"
    <|> CFlat   <$ reserved "Cb"
    <|> GFlat   <$ reserved "Gb"
    <|> DFlat   <$ reserved "Db"
    <|> AMinor  <$ reserved "a"
    <|> EMinor  <$ reserved "e"
    <|> BMinor  <$ reserved "b"
    <|> AFMinor <$ reserved "ab"
    <|> EFMinor <$ reserved "eb"
    <|> BFMinor <$ reserved "bb"
    <|> FMinor  <$ reserved "f"
    <|> CMinor  <$ reserved "c"
    <|> GMinor  <$ reserved "g"
    <|> DMinor  <$ reserved "d"

{-  I initially used roman numerals for triads and sevenths, 
    but felt like that just made it more troublesome.  -}

parseTriad :: Parser ETriad
parseTriad = T1 <$> try (parseRhythm <* reserved "1")
    <|> T2 <$> try (parseRhythm <* reserved "2")
    <|> T3 <$> try (parseRhythm <* reserved "3")
    <|> T4 <$> try (parseRhythm <* reserved "4")
    <|> T5 <$> try (parseRhythm <* reserved "5")
    <|> T6 <$> try (parseRhythm <* reserved "6")
    <|> T7 <$> try (parseRhythm <* reserved "7")

parseSeventh :: Parser ESeventh
parseSeventh = S1 <$> try (parseRhythm <* reserved "1")
    <|> S2 <$> try (parseRhythm <* reserved "2")
    <|> S3 <$> try (parseRhythm <* reserved "3")
    <|> S4 <$> try (parseRhythm <* reserved "4")
    <|> S5 <$> try (parseRhythm <* reserved "5")
    <|> S6 <$> try (parseRhythm <* reserved "6")
    <|> S7 <$> try (parseRhythm <* reserved "7")

parseChord :: Parser EChord
parseChord = Custom <$ reserved "chord" <*> (symbol "(" *> parseGroupC <* symbol ")")
    <|> (Triad <$ reserved "tri" <*> parseTriad <|> Triad <$ reserved "triad" <*> parseTriad)
    <|> (Seventh <$ reserved "sev" <*> parseSeventh <|> Seventh <$ reserved "seventh" <*> parseSeventh)

parseAtom :: Parser Expr 
parseAtom = Chord <$> parseChord
    <|> TimeSig <$> integer <* symbol "/" <*> integer
    <|> Note    <$> try parseRhythm <*> parsePitch
    <|> MNotes  <$ symbol "(" <*> parseGroupC <* symbol ")"
    <|> Key     <$> parseScale
    <|> Meas    <$ try (reserved "measure" <* symbol "{") <*> (parseGroupS <* symbol "}")
    <|> Block   <$ try (reserved "block" <* symbol "{") <*> (parseGroupS <* symbol "}")
    <|> Var     <$> identifier

parseExpr :: Parser Expr 
parseExpr = buildExpressionParser table parseAtom 
    where 
        table = [   [Infix (Bin Tie <$ reservedOp "->") AssocLeft 
                    ]
                ]

parseType :: Parser Type
parseType = (TyNote <$ reserved "note")
    <|> (TyMeas <$ reserved "measure" <|> TyMeas <$ reserved "block")

parseStmt :: Parser Stmt
parseStmt = Assign <$> try (identifier <* reservedOp "=") <*> parseExpr
    <|> StExpr <$> parseExpr
    <|> Decl   <$> parseType <*> identifier

parseProg :: Parser Prog
parseProg = parseStmt `endBy` reservedOp ";"

mcomp :: Parser Prog
mcomp = whiteSpace *> parseProg <* eof

parseCheck :: FilePath -> IO ()  -- Modeled after the typecheck from Module 11
parseCheck fileName = do
    s <- readFile fileName
    case parse mcomp s of
        Left err -> print err
        Right p  -> print p


-- Type-Checking: 
----------------------------------------------------------------------------------------

-- Chords, Triads, and Sevenths all should be represented as notes.
-- Blocks should be all measures.


data TypeError where
    UsedVar     :: Var -> TypeError
    Undefined   :: Var -> TypeError
    Mismatch    :: Expr -> Expr -> Type -> Type -> TypeError
    LonelyNote  :: Expr -> TypeError
    Invalid     :: Expr -> TypeError
    BadTie      :: Expr -> Expr -> Type -> TypeError
    deriving Show

printType :: Type -> String 
printType x = 
    case x of 
        TyNote -> "note"
        TyMeas -> "meas"
        TyComm -> "command"

showTyError :: TypeError -> String
showTyError (UsedVar x)              = unlines 
    [ ""
    , show x ++ " has already been defined."
    ]
showTyError (Undefined x)            = unlines 
    [ ""
    , show x ++ " has not been defined."
    ]
showTyError (Mismatch eRoot e t1 t2) = unlines 
    [ ""
    , "Type mismatch in the expression: " ++ printExpr e
    , ""
    , "Parent expression: " ++ printExpr eRoot
    , ""
    , " Expected type: " ++ printType t1
    , " Actual type:   " ++ printType t2
    ] 
showTyError (Invalid e)              = unlines 
    [ ""
    , "Invalid expression: " ++ printExpr e
    ]
showTyError (LonelyNote e)           = unlines 
    [ ""
    , "The following note(s) must be contained within a measure: "
    , "  " ++ printExpr e
    ]
showTyError (BadTie eRoot e t)       = unlines 
    [ ""
    , "Type error in the tie expression: " ++ printExpr eRoot
    , ""
    , " " ++ printExpr e ++ " is expected to be a note or measure, but is a " ++ printType t
    ]

type Ctx = M.Map Var Type

infer :: Ctx -> Expr -> Either TypeError Type
infer _   (Note _ _)               = Right TyNote
infer ctx (MNotes x)               = progType (MNotes x) TyNote ctx
infer ctx (Meas x)                 = progType (Meas x) TyNote ctx
infer ctx (Block x)                = progType (Block x) TyMeas ctx
infer ctx (Chord (Custom x))       = progType (Chord (Custom x)) TyNote ctx
infer _   (Chord (Triad _))        = Right TyNote
infer _   (Chord (Seventh _))      = Right TyNote
infer ctx (Bin Tie e1 e2)          =  
    case infer ctx e1 of
        Right TyNote -> check ctx (Bin Tie e1 e2) e2 TyNote *> Right TyNote
        Right TyMeas -> check ctx (Bin Tie e1 e2) e2 TyMeas *> Right TyMeas
        Right y      -> Left $ BadTie (Bin Tie e1 e2) e1 y
        Left err     -> Left err
infer ctx (Var x)                  =
    case M.lookup x ctx of
        Nothing -> Left $ Undefined x
        Just t  -> Right t
infer _   _                        = Right TyComm

check :: Ctx -> Expr -> Expr -> Type -> Either TypeError ()
check ctx eRoot e ty = infer ctx e >>= \ty' ->
    if ty == ty' 
    then Right () 
    else Left $ Mismatch eRoot e ty ty'

progType :: Expr -> Type -> Ctx -> Either TypeError Type
progType (MNotes []) _ _                      = Right TyNote
progType (MNotes (x:xs)) ty ctx               = 
    check ctx (MNotes (x:xs)) x ty 
    *> progType (MNotes xs) ty ctx
progType (Meas []) _ _                        = Right TyMeas
progType (Meas (x:xs)) ty ctx                 = 
    check ctx (Meas (x:xs)) x ty 
    *> progType (Meas xs) ty ctx
progType (Block []) _ _                       = Right TyMeas
progType (Block (x:xs)) ty ctx                = 
    check ctx (Block (x:xs)) x ty 
    *> progType (Block xs) ty ctx
progType (Chord (Custom [])) _ _              = Right TyNote
progType (Chord (Custom (x:xs))) ty ctx       = 
    check ctx (Chord (Custom (x:xs))) x ty 
    *> progType (Chord (Custom xs)) ty ctx
progType e _ _                                = Left $ Invalid e


{-  I wanted to represent timeSig and key as variables without having the user
    to define them as such themselves. I struggled to figure out a good way to
    do that, until I realized that I could just force a declaration before 
    interpretting the progression of statements. Representing them as variables 
    allowed for the odd input of `timeSig;`, which I fixed in checkStmt.  
    
    Furthermore, if the user does not assign timeSig or key, I wanted them
    to default to 4/4 and C Major, respectively. I did this by creating a progression
    of all the statements that occurred before the first StExpr. It then iterates through
    looking for assignments for key and timeSig. If either or both are not found, it
    assigns it, then proceeds with interpretting the original progression.  -}


checkProg :: Ctx -> Prog -> Either TypeError Ctx 
checkProg ctx []     = Right ctx 
checkProg ctx (x:xs) = checkStmt ctx x >>= \ctx' -> checkProg ctx' xs

checkStmt :: Ctx -> Stmt -> Either TypeError Ctx 
checkStmt ctx (Decl ty x)  = 
    case M.lookup x ctx of 
        Nothing -> Right (M.insert x ty ctx) 
        Just _  -> Left (UsedVar x)
checkStmt ctx (Assign x e) = 
    case M.lookup x ctx of 
        Nothing      -> Left $ Undefined x
        Just y       -> check ctx (Var x) e y *> Right ctx 
checkStmt ctx (StExpr e)   = 
    case infer ctx e of 
        Right TyMeas -> Right ctx
        Right TyComm -> Left (Invalid e)
        Right TyNote -> Left (LonelyNote e)
        Left err     -> Left err

autoAssign :: Prog -> Prog -> Prog -> Prog
autoAssign _ []     p = p
autoAssign r (x:xs) p = 
    case x of 
        (StExpr _) -> 
            let p' = checkTimeSig r p 
            in checkKey r p'
        _          -> autoAssign (r ++ [x]) xs p

checkTimeSig :: Prog -> Prog -> Prog 
checkTimeSig []     p = Assign "timeSig" (TimeSig 4 4) : p
checkTimeSig (x:xs) p = 
    case x of 
        (Assign "timeSig" _) -> p
        _                    -> checkTimeSig xs p

checkKey :: Prog -> Prog -> Prog 
checkKey [] p     = Assign "key" (Key CMajor) : p
checkKey (x:xs) p = 
    case x of 
        (Assign "key" _) -> p
        _                -> checkKey xs p


typeCheck :: FilePath -> IO ()  -- For testing type checking; adapted from Module 11
typeCheck fileName = do
    s <- readFile fileName
    case parse mcomp s of
        Left err -> print err
        Right p  ->
            case checkProg M.empty (Decl TyComm"timeSig":Decl TyComm"key":Assign"timeSig"(TimeSig 4 4):Assign"key"(Key CMajor):p) of
                Left tyErr -> putStrLn (showTyError tyErr)
                Right _    -> putStrLn "Typechecked successfully."


-- Interpreter: 
----------------------------------------------------------------------------------------


{-  I would love to be able to parse the user's code and pretty-print sheet music,
    but that seems like it would be by far the most time consuming part of this project.
    Instead, I could just print full measures and important commands (timeSig and key) 
    line-by-line, but that result almost seems more painful than the work it would take
    to pretty-print sheet music. -}


data InterpError where 
    WrongBeat    :: Expr -> Double -> Double -> InterpError
    WrongChord   :: Expr -> InterpError
    ChordInChord :: Expr -> InterpError
    BadTimeSig   :: Expr -> Integer -> InterpError
    deriving Show

showInterpError :: InterpError -> String
showInterpError (WrongBeat e x y) = unlines 
    [ ""
    , "Incorrect number of beats in the following expression: " ++ printExpr e
    , " Expected: " ++ show x
    , " Actual:   " ++ show y
    ]
showInterpError (WrongChord root)  = unlines
    [ ""
    , "The following is not a valid chord: "
    , " " ++ printExpr root
    ]
showInterpError (ChordInChord e) = unlines
    ["" 
    , "The following chord cannot contain another chord: "
    , " " ++ printExpr e
    ]
showInterpError (BadTimeSig e x) = unlines 
    [""
    , printExpr e ++ " is not a valid time signature. " ++ show x 
    ]

type Mem = M.Map Var Expr 

validateTimeSig :: Expr -> Either InterpError ()
validateTimeSig (TimeSig y x) = 
    case x of 
        1  -> Right ()
        2  -> Right ()
        4  -> Right ()
        8  -> Right ()
        16 -> Right ()
        32 -> Right ()
        _  -> Left (BadTimeSig (TimeSig y x) x)
validateTimeSig x             = error $ "Case expression error in the expression: " ++ show x

interpProg :: Mem -> Group -> Prog -> Either InterpError Group 
interpProg _   grp []                  = Right grp
interpProg mem grp (Decl _ _ : rest)   = interpProg mem grp rest
interpProg mem grp (Assign x e : rest) = 
    case x of 
        "timeSig" -> validateTimeSig e *> interpProg (M.insert x e mem) (grp ++ [e]) rest
        "key"     -> interpProg (M.insert x e mem) (grp ++ [e]) rest
        _         -> interpProg (M.insert x e mem) grp rest
interpProg mem grp (StExpr e : rest)   = 
    case e of  -- Case expression in order to pull measures from blocks
        (Block [])      -> interpProg mem grp rest
        (Block (x:xs))  -> interpProg mem grp (StExpr x : StExpr (Block xs) : rest) 
        x               -> interpExpr mem x [] >>= \x' -> interpProg mem (grp ++ [x']) rest


{-  I should have made two subtypes of Expr that would distinguish between
    measures and notes. This would eliminate the need to leave a wildcard
    case expression in the following function (and many other places).  -}


interpExpr :: Mem -> Expr -> Group -> Either InterpError Expr
interpExpr mem (Var x) meas          =             
    case M.lookup x mem of
        Just y  -> interpExpr mem y meas
        Nothing -> error $ "Impossible! Uninitialized variable: " ++ show x
interpExpr mem (Meas []) meas        = 
    case M.lookup "timeSig" mem of 
        Just (TimeSig x y) -> 
            let x' = fromIntegral x 
            in  let y' = fromIntegral y
                in checkMeasure x' y' 0 (Meas meas) (Meas meas) *> Right (Meas meas)
        Just x             -> error $ "Time Signature has been assigned to the expression " ++ show x
        Nothing            -> error "Time Signature did not default to 4/4 as intended. "
interpExpr mem (Meas (x:xs)) meas    = 
    case x of 
        (MNotes y)    -> elimMNotes mem y [] >>= \y' -> 
            interpExpr mem (Meas xs) (meas ++ y')
        _             -> interpExpr mem x meas >>= \x' -> interpExpr mem (Meas xs) (meas ++ [x'])
interpExpr mem (Bin Tie x y) _       = interpExpr mem x [] >>= \x' ->
    interpExpr mem y [] >>= \y' ->
    Right (Bin Tie x' y') 
interpExpr _ (Note r p)           _  = Right (Note r p)
interpExpr mem (Chord (Custom x)) _  = interpGroup mem [] x >>= \grp -> 
    Right (Chord (Custom grp))
interpExpr mem (Chord (Triad x))  _  = 
    interpExpr mem (Chord (Custom (interpTriad mem x))) []
interpExpr mem (Chord (Seventh x)) _ = 
    interpExpr mem (Chord (Custom (interpSeventh mem x))) []
interpExpr _ x _ = error $ "Case expression error:" ++ printExpr x

interpGroup :: Mem -> Group -> Group -> Either InterpError Group
interpGroup _   grp []     = Right grp
interpGroup mem grp (x:xs) = interpExpr mem x [] >>= \x' ->
    interpGroup mem (grp ++ [x']) xs

elimMNotes :: Mem -> Group -> Group -> Either InterpError Group 
elimMNotes _ []       stk = Right stk
elimMNotes mem (x:xs) stk = interpExpr mem x [] >>= \x' -> elimMNotes mem xs (stk ++ [x'])

-- Just need the base pitch for calculating triads and sevenths
scaleToPitch :: Scale -> Pitch 
scaleToPitch x = 
    case x of 
        AMajor  -> A Nothing 
        AFlat   -> A Nothing 
        AMinor  -> A Nothing 
        ASMinor -> A Nothing 
        AFMinor -> A Nothing 
        BMajor  -> B Nothing
        BFlat   -> B Nothing 
        BMinor  -> B Nothing 
        BFMinor -> B Nothing 
        CMajor  -> C Nothing
        CFlat   -> C Nothing
        CMinor  -> C Nothing
        CSMinor -> C Nothing
        CSharp  -> C Nothing
        DMajor  -> D Nothing
        DFlat   -> D Nothing
        DMinor  -> D Nothing
        DSMinor -> D Nothing
        EMajor  -> E Nothing
        EFlat   -> E Nothing
        EMinor  -> E Nothing
        EFMinor -> E Nothing
        FMajor  -> F Nothing
        FMinor  -> F Nothing
        FSMinor -> F Nothing
        FSharp  -> F Nothing
        GMajor  -> G Nothing
        GFlat   -> G Nothing
        GMinor  -> G Nothing
        GSMinor -> G Nothing

getTonic :: Mem -> Pitch  
getTonic mem = 
    case M.lookup "key" mem of 
        Just (Key x)  -> scaleToPitch x
        Just x        -> error $ "Key has been assigned to the expression " ++ show x
        Nothing       -> error "Key did not default to C Major as intended. "

pitchValue :: Pitch -> Integer 
pitchValue (A Nothing) = 1
pitchValue (B Nothing) = 2
pitchValue (C Nothing) = 3
pitchValue (D Nothing) = 4
pitchValue (E Nothing) = 5
pitchValue (F Nothing) = 6
pitchValue (G Nothing) = 7
pitchValue _           = -100

intToPitch :: Integer -> Pitch 
intToPitch 1 = A Nothing
intToPitch 2 = B Nothing
intToPitch 3 = C Nothing
intToPitch 4 = D Nothing
intToPitch 5 = E Nothing
intToPitch 6 = F Nothing
intToPitch 7 = G Nothing
intToPitch x = intToPitch (x - 7)

interpTriad :: Mem -> ETriad -> Group 
interpTriad mem x = 
    let y = pitchValue (getTonic mem) 
    in case x of 
        T1 r ->   [ Note r (intToPitch y)
                    , Note r (intToPitch (y+2))
                    , Note r (intToPitch (y+4)) ]
        T2 r ->   [ Note r (intToPitch (y+1))
                    , Note r (intToPitch (y+3))
                    , Note r (intToPitch (y+5)) ]
        T3 r ->   [ Note r (intToPitch (y+2))
                    , Note r (intToPitch (y+4))
                    , Note r (intToPitch (y+6)) ]
        T4 r ->   [ Note r (intToPitch (y+3))
                    , Note r (intToPitch (y+5))
                    , Note r (intToPitch (y+7)) ]
        T5 r ->   [ Note r (intToPitch (y+4))
                    , Note r (intToPitch (y+6))
                    , Note r (intToPitch (y+8)) ]
        T6 r ->   [ Note r (intToPitch (y+5))
                    , Note r (intToPitch (y+7))
                    , Note r (intToPitch (y+9)) ]
        T7 r ->   [ Note r (intToPitch (y+6))
                    , Note r (intToPitch (y+8))
                    , Note r (intToPitch (y+10)) ]

interpSeventh :: Mem -> ESeventh -> Group
interpSeventh mem x = 
    let y = pitchValue (getTonic mem) 
    in case x of 
        S1 r ->   [ Note r (intToPitch y)
                    , Note r (intToPitch (y+2))
                    , Note r (intToPitch (y+4))
                    , Note r (intToPitch (y+6)) ]
        S2 r ->   [ Note r (intToPitch (y+1))
                    , Note r (intToPitch (y+3))
                    , Note r (intToPitch (y+5)) 
                    , Note r (intToPitch (y+7)) ]
        S3 r ->   [ Note r (intToPitch (y+2))
                    , Note r (intToPitch (y+4))
                    , Note r (intToPitch (y+6)) 
                    , Note r (intToPitch (y+8)) ]
        S4 r ->   [ Note r (intToPitch (y+3))
                    , Note r (intToPitch (y+5))
                    , Note r (intToPitch (y+7)) 
                    , Note r (intToPitch (y+9)) ]
        S5 r ->   [ Note r (intToPitch (y+4))
                    , Note r (intToPitch (y+6))
                    , Note r (intToPitch (y+8)) 
                    , Note r (intToPitch (y+10)) ]
        S6 r ->   [ Note r (intToPitch (y+5))
                    , Note r (intToPitch (y+7))
                    , Note r (intToPitch (y+9)) 
                    , Note r (intToPitch (y+11)) ]
        S7 r ->   [ Note r (intToPitch (y+6))
                    , Note r (intToPitch (y+8))
                    , Note r (intToPitch (y+10)) 
                    , Note r (intToPitch (y+12)) ]

-- Where `x` is the denominator of the time signature
calcRhythm :: Double -> Rhythm -> Double 
calcRhythm x r = 
    case r of 
        Whole     -> x
        WDot      -> x + (x * (1/2))
        WDoubDot  -> calcRhythm x WDot + (x * (1/4))
        Half      -> x / 2
        HDot      -> x / 2 + (calcRhythm x Half * (1/2))
        HDoubDot  -> calcRhythm x HDot + (calcRhythm x Half * (1/4))
        Quarter   -> x / 4
        QDot      -> x / 4 + (calcRhythm x Quarter * (1/2))
        QDoubDot  -> calcRhythm x QDot + (calcRhythm x Quarter * (1/4))
        Eighth    -> x / 8
        EDot      -> x / 8 + (calcRhythm x Eighth * (1/2))
        EDoubDot  -> calcRhythm x EDot + (calcRhythm x Eighth * (1/4))
        Sixteenth -> x / 16
        SDot      -> x / 16 + (calcRhythm x Sixteenth * (1/2))
        SDoubDot  -> calcRhythm x SDot + (calcRhythm x Sixteenth * (1/4))
        ThirtySec -> x / 32
        TSDot     -> x / 32 + (calcRhythm x ThirtySec * (1/2))
        TSDoubDot -> calcRhythm x TSDot + (calcRhythm x ThirtySec * (1/4))

calcNote :: Expr -> Expr -> Double -> Either InterpError Double 
calcNote _    (Note r _)    beat = Right (calcRhythm beat r)
calcNote _    (Chord z)     beat = checkChord (Chord z) z beat 0
calcNote root (Bin Tie x y) beat = 
    calcNote root x beat >>= \x' ->
    calcNote root y beat >>= \y' ->
        Right (x' + y')
calcNote _ x _                       = error $ "Case expression error in the expression: " ++ show x

checkChord :: Expr -> EChord -> Double -> Double -> Either InterpError Double
checkChord _    (Custom [])     _    prev = Right prev
checkChord root (Custom (x:xs)) beat prev = 
    case x of 
        Chord _       -> Left (ChordInChord root)
        Bin Tie z1 z2 -> 
            case z1 of 
                Chord _ -> Left (ChordInChord root)
                _       ->
                    case z2 of 
                        Chord _  -> Left (ChordInChord root)
                        _        -> 
                            calcNote root (Bin Tie z1 z2) beat >>= \z' -> 
                                if z' /= prev && prev /= 0
                                then Left (WrongChord root) -- Wrong very wrong
                                else checkChord root (Custom xs) beat z'
        z             -> 
            calcNote root z beat >>= \z' -> 
                if z' /= prev && prev /= 0
                then Left (WrongChord root) 
                else checkChord root (Custom xs) beat z'
checkChord x _ _ _                        = error $ "Case expression error in the expression: " ++ show x

checkMeasure :: Double -> Double -> Double -> Expr -> Expr -> Either InterpError ()
checkMeasure numBeat beat total (Meas grp) meas = 
    case grp of 
        []     -> 
            if total == numBeat
            then Right ()
            else Left (WrongBeat meas numBeat total)
        (y:ys) -> calcNote (Meas grp) y beat >>= \val ->
            checkMeasure numBeat beat (val + total) (Meas ys) meas
checkMeasure _ _ _ x _                          = error $ "Case expression error in the expression: " ++ show x


-- Pretty-Printing: 
----------------------------------------------------------------------------------------


prettyPrint :: [String] -> String -> String 
prettyPrint []     final = final
prettyPrint (x:xs) final = final ++ x ++ "\n\n" ++ prettyPrint xs final

printExpr :: Expr -> String
printExpr x = 
    case x of 
        TimeSig y z       -> "Time Signature: " ++ show y ++ "/" ++ show z
        Key y             -> "Key: " ++ printScale y
        Meas grp          -> measToString (Meas grp) "[" ++ "]"
        Block mGrp        -> printBlock (Block mGrp) ""
        Note  r p         -> printRhythm r ++ " " ++ printPitch p
        MNotes grp        -> printMNotes (MNotes grp) ""
        Chord (Custom y)  -> chordToString (Custom y) ""
        Chord (Triad y)   -> printETriad y 
        Chord (Seventh y) -> printESeventh y
        Var y             -> "Var: " ++ show y  -- probably wrong
        Bin Tie y z       -> printExpr y ++ " -> " ++ printExpr z

printMNotes :: Expr -> String -> String 
printMNotes (MNotes [x])    str = "MNotes: ( " ++ (str ++ printExpr x) ++ ")"
printMNotes (MNotes (x:xs)) str = printMNotes (MNotes xs) (str ++ printExpr x ++ ", ")
printMNotes _               str = "Case expression error: " ++ str

printBlock :: Expr -> String -> String 
printBlock (Block [x])    str = "Block: [" ++ (str ++ printExpr x) ++ "]"
printBlock (Block (x:xs)) str = printBlock (Block xs) (str ++ printExpr x ++ ", ")
printBlock _              str = "Case expression error: " ++ str

printETriad :: ETriad -> String 
printETriad x = 
    case x of 
        T1 r -> printRhythm r ++ " 1st triad"
        T2 r -> printRhythm r ++ " 2nd triad"
        T3 r -> printRhythm r ++ " 3rd triad"
        T4 r -> printRhythm r ++ " 4th triad"
        T5 r -> printRhythm r ++ " 5th triad"
        T6 r -> printRhythm r ++ " 6th triad"
        T7 r -> printRhythm r ++ " 7th triad"

printESeventh :: ESeventh -> String 
printESeventh x = 
    case x of 
        S1 r -> printRhythm r ++ " 1st seventh"
        S2 r -> printRhythm r ++ " 2nd seventh"
        S3 r -> printRhythm r ++ " 3rd seventh"
        S4 r -> printRhythm r ++ " 4th seventh"
        S5 r -> printRhythm r ++ " 5th seventh"
        S6 r -> printRhythm r ++ " 6th seventh"
        S7 r -> printRhythm r ++ " 7th seventh"

exprToString :: Group -> [String] -> [String]
exprToString []     stk = stk 
exprToString (x:xs) stk = exprToString xs (stk ++ [printExpr x])

stringFromStk :: [String] -> String  -- To help with formatting Bin Tie [measure] [measure]
stringFromStk (x:_) = x
stringFromStk x     = error $ "Case expression error in the expression: " ++ show x

measToString :: Expr -> String -> String
measToString (Meas [])     meas = meas
measToString (Meas (x:xs)) meas = 
    case xs of 
        [] -> meas ++ (printExpr x ++ measToString (Meas xs) "")
        _  -> meas ++ (printExpr x ++ ", " ++ measToString (Meas xs) "")
measToString x             _    = error $ "Case expression error in the expression: " ++ show x
 
chordToString :: EChord -> String -> String 
chordToString (Custom [x])    full = "(" ++ full ++ printExpr x ++ ")" 
chordToString (Custom (x:xs)) full = chordToString (Custom xs) (full ++ printExpr x ++ ", ")
chordToString x               _    = error $ "Case expression error in the expression: " ++ show x

printScale :: Scale -> String 
printScale x = 
    case x of
        AMajor  -> "A Major"
        AFlat   -> "A Flat Major" 
        BMajor  -> "B Major"
        BFlat   -> "B Flat Major" 
        CMajor  -> "C Major"
        CFlat   -> "C Flat Major"
        CSharp  -> "C Sharp Major"
        DMajor  -> "D Major"
        DFlat   -> "D Flat Major"
        EMajor  -> "E Major"
        EFlat   -> "E Flat Major"
        FMajor  -> "F Major"
        FSharp  -> "F Sharp Major"
        GMajor  -> "G Major"
        GFlat   -> "G Flat Major"
        AMinor  -> "A Minor"
        ASMinor -> "A Sharp Minor"
        AFMinor -> "A Flat Minor"
        BMinor  -> "B Minor"
        BFMinor -> "B Flat Minor"
        CMinor  -> "C Minor"
        CSMinor -> "C Sharp Minor"
        DMinor  -> "C Minor"
        DSMinor -> "D Sharp Minor"
        EMinor  -> "E Minor"
        EFMinor -> "E Flat Minor"
        FMinor  -> "F Minor"
        FSMinor -> "F Sharp Minor"
        GMinor  -> "G Minor"
        GSMinor -> "F Sharp Minor"

printRhythm :: Rhythm -> String 
printRhythm r = 
    case r of 
        ThirtySec -> "32nd"
        Sixteenth -> "16th"
        Eighth    -> "8th"
        Quarter   -> "Quarter"
        Half      -> "Half"
        Whole     -> "Whole"
        TSDot     -> "Dotted 32nd"
        SDot      -> "Dotted 16th"
        EDot      -> "Dotted 8th"
        QDot      -> "Dotted Quarter"
        HDot      -> "Dotted Half"
        WDot      -> "Dotted Whole"
        TSDoubDot -> "Double-dotted 32nd"
        SDoubDot  -> "Double-dotted 16th"
        EDoubDot  -> "Double-dotted 8th"
        QDoubDot  -> "Double-dotted Quarter"
        HDoubDot  -> "Double-dotted Half"
        WDoubDot  -> "Double-dotted Whole"

printPitch :: Pitch -> String 
printPitch p = 
    case p of 
        Rest               -> "rest"
        A Nothing          -> "A"
        A (Just Flat)      -> "Ab"
        A (Just DoubFlat)  -> "Abb"
        A (Just Sharp)     -> "A#"
        A (Just DoubSharp) -> "A##"
        A (Just Nat)       -> "An"
        B Nothing          -> "B"
        B (Just Flat)      -> "Bb"
        B (Just DoubFlat)  -> "Bbb"
        B (Just Sharp)     -> "B#"
        B (Just DoubSharp) -> "B##"
        B (Just Nat)       -> "Bn"
        C Nothing          -> "C"
        C (Just Flat)      -> "Cb"
        C (Just DoubFlat)  -> "Cbb"
        C (Just Sharp)     -> "C#"
        C (Just DoubSharp) -> "C##"
        C (Just Nat)       -> "Cn"
        D Nothing          -> "D"
        D (Just Flat)      -> "Db"
        D (Just DoubFlat)  -> "Dbb"
        D (Just Sharp)     -> "D#"
        D (Just DoubSharp) -> "D##"
        D (Just Nat)       -> "Dn"
        E Nothing          -> "E"
        E (Just Flat)      -> "Eb"
        E (Just DoubFlat)  -> "Ebb"
        E (Just Sharp)     -> "E#"
        E (Just DoubSharp) -> "E##"
        E (Just Nat)       -> "En"
        F Nothing          -> "F"
        F (Just Flat)      -> "Fb"
        F (Just DoubFlat)  -> "Fbb"
        F (Just Sharp)     -> "F#"
        F (Just DoubSharp) -> "F##"
        F (Just Nat)       -> "Fn"
        G Nothing          -> "G"
        G (Just Flat)      -> "Gb"
        G (Just DoubFlat)  -> "Gbb"
        G (Just Sharp)     -> "G#"
        G (Just DoubSharp) -> "G##"
        G (Just Nat)       -> "Gn"


run :: String -> IO ()
run fileName = do
    s <- readFile (fileName ++ ".mcomp")
    case parse mcomp s of
        Left err -> print err
        Right p  -> 
            let p' = (Decl TyComm "timeSig" : Decl TyComm "key" : p) 
            in case checkProg M.empty p' of
                Left tyErr -> putStrLn (showTyError tyErr)
                Right _    -> 
                    case interpProg M.empty [] (autoAssign [] p' p') of
                        Left err -> putStrLn (showInterpError err)
                        Right y  -> putStrLn ("\n" ++ prettyPrint (exprToString y []) "") 
