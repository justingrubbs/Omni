module Deprecated.Python
   (
      pyParse, checkPyParse, printPython
   )
   where

import            Control.Applicative        (empty)
import            Control.Monad              (void)
import            Control.Monad.Combinators.Expr
import            Text.Megaparsec
import            Text.Megaparsec.Char
import qualified  Text.Megaparsec.Char.Lexer as L
import            Omni.Data
import            Data.Void


type Parser = Parsec Void String

-- Some of the basic code/structure taken from:
   -- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
   -- https://github.com/mrkkrp/megaparsec-site/blob/master/tutorials/parsing-simple-imperative-language.md
   -- https://github.com/disco-lang/disco/tree/master/src/Disco

-- IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- https://hackage.haskell.org/package/language-python-0.5.8/docs/Language-Python-Common-AST.html#t:ModuleSpan
-- IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

-- Lexing
---------------------------------------------------------------------

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()  -- Space consumer that does not consume new lines (only parses spaces and tabs)
sc = L.space (void $ oneOf " \t") lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reservedOp :: String -> Parser ()
reservedOp s = (lexeme . try) (string s *> notFollowedBy (oneOf opChar))

opChar :: [Char]
opChar = "~!@#$%^&*-+=|<>?/\\.'\""

integer :: Parser Integer
integer = lexeme L.decimal -- is this correct?

float :: Parser Float
float = lexeme L.float

-- Notable: Python allows multi-line strings with triple quotes or something
parens, curly, angles, brackets, sQuotes, dQuotes :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
curly    = between (symbol "{") (symbol "}")
angles   = between (symbol "<") (symbol ">")
brackets = between (symbol "[") (symbol "]")
sQuotes  = between (symbol "'") (symbol "'")
dQuotes  = between (symbol "\"") (symbol "\"")

semi, comma, colon, dot, pipe, hash :: Parser String
semi  = symbol ";"
comma = symbol ","
colon = symbol ":"
dot   = symbol "."
pipe  = symbol "|"
hash  = symbol "#"

reserved :: String -> Parser ()
reserved w = (lexeme . try) $ string w *> notFollowedBy alphaNumChar

reservedWords :: [String]
reservedWords = ["print", "int", "False", "True", "bool", "boolean", "if", "elif", "else", "and", "or", "not"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
   where
      p       = (:) <$> letterChar <*> many alphaNumChar
      check x =
         if x `elem` reservedWords
         then fail $ "keyword " ++ show x ++ " cannot be an identifier."
         else return x

-- Parsing
---------------------------------------------------------------------

parseType :: Parser Type
parseType = TyInt <$ reserved "int"
   <|> TyBool <$ (reserved "boolean" <|> reserved "bool")

parseExprAtom :: Parser Expr
parseExprAtom = EInt <$> integer
   <|> (EBool True <$ reserved "True" <|> EBool False <$ reserved "False")
   <|> EVar <$> identifier
   <|> parens parseExpr
   <|> parseNot -- Believe this must be below parens parseExpr because of precedence, think and and or should be below not though

parseExpr :: Parser Expr 
parseExpr = makeExprParser parseExprAtom operators 

parseNot :: Parser Expr 
parseNot = Un Not <$> (reserved "not" *> parseExpr)

operators :: [[Operator Parser Expr]]
operators = 
   [  [ Prefix (Un Neg <$ symbol "-") ]

   ,  [ InfixL (Bin Mul <$ symbol "*")
      , InfixL (Bin Div <$ symbol "/") 
      , InfixL (Bin Mod <$ symbol "%") ]

   ,  [ InfixL (Bin Add <$ symbol "+")
      , InfixL (Bin Sub <$ symbol "-") ]

   ,  [ InfixL (Bin LessEq <$ reservedOp "<=")
      , InfixL (Bin GreaterEq <$ reservedOp ">=")
      , InfixL (Bin Less <$ symbol "<") 
      , InfixL (Bin Greater <$ symbol ">") ] -- Doesn't work for some reason

   ,  [ InfixL (Bin Eq <$ reservedOp "==") 
      , InfixL (Bin NEq <$ reservedOp "!=") ]

   ,  [ InfixL (Bin And <$ reserved "and") ]

   ,  [ InfixL (Bin Or <$ reserved "or") ]

   ]

parseStmt :: Parser Stmt
parseStmt = Output <$> (reserved "print" *> parens parseExpr)
      <|> try (Assign <$> (identifier <* colon <* parseType) <*> (reservedOp "=" *> parseExpr))
      <|> Assign <$> identifier <*> (reservedOp "=" *> parseExpr)

parseProg :: Parser Prog
-- parseProg = sepEndBy parseStmt eol
parseProg = sepEndBy parseStmt (char '\n')   -- Works as well for my tests thus far, do these functionally differ

parseDoc :: Parser Prog
parseDoc = between sc eof parseProg

oParse :: Parser a -> String -> Either (ParseErrorBundle String Void) a
oParse p = parse p ""

pyParse :: String -> Either (ParseErrorBundle String Void) Prog
pyParse = oParse parseDoc

checkPyParse :: FilePath -> IO ()
checkPyParse file = do
   s <- readFile file
   case pyParse s of
      Left err -> print err
      Right p  -> print p


-- For expression table:
   -- Only parse unary operators consisting of operator symbols.
   -- Alphabetic unary operators (i.e. 'not') will be parsed as
   -- applications of variable names, since if they are parsed here
   -- they will incorrectly parse even when they are a prefix of a
   -- variable name.


-- Pretty-Printing:
---------------------------------------------------------------------

-- Will be used to add def main(): to beginning and main() and such.
printPython :: Prog -> String 
printPython = prettyPython ""

prettyPython :: String -> Prog -> String
prettyPython = foldl (\ text x -> text ++ printStmt x ++ "\n")

printStmt :: Stmt -> String
printStmt (Assign var e)     = var ++ " = " ++ printExpr e
printStmt (Declare ty var)   = var ++ ": " ++ printType ty
printStmt (DAndA ty var e)   = var ++ ": " ++ printType ty ++ " = " ++ printExpr e
printStmt (Output e)         = "print(" ++ printExpr e ++ ")"

printType :: Type -> String
printType TyInt  = "int"
printType TyBool = "bool"
printType Poly   = "<Type unknown>"
printType (TVar t) = "Type variable: " ++ show t 

printExpr :: Expr -> String
printExpr (EInt n)      = show n
printExpr (EBool True)  = "True"
printExpr (EBool False) = "False"
printExpr (EVar v)      = v
printExpr (Bin bop x y) = printExpr x ++ printBop bop ++ printExpr y
printExpr (Un uop x)    = printUop uop ++ printExpr x

printBop :: BOp -> String 
printBop Add = " + "
printBop Sub = " - "
printBop Mul = " * "
printBop Div = " / "
printBop Mod = " % "
printBop And = " and "
printBop Or  = " or "
printBop Eq  = " == "
printBop NEq = " != "
printBop Less = " < "
printBop Greater = " > "
printBop LessEq = " <= "
printBop GreaterEq = " >= "

printUop :: UOp -> String 
printUop Not = "not "
printUop Neg = "-"

