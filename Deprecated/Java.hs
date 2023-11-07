module Deprecated.Java
   (
      javaParse, checkJavaParse, printJava
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

-- Lexing
---------------------------------------------------------------------

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

scn :: Parser ()  -- Space consumer that consumes newlines
scn = L.space (void spaceChar) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: String -> Parser String
symbol = L.symbol scn

reservedOp :: String -> Parser ()
reservedOp s = (lexeme . try) (string s *> notFollowedBy (oneOf opChar))

opChar :: [Char]
opChar = "~!@#$%^&*-+=|<>?/\\.'\""

pChar :: Parser Char 
pChar = lexeme L.charLiteral

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Float
float = lexeme L.float

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
reservedWords = ["int", "System", "out", "println", "true", "false", "boolean", "if", "else", "String", "char"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
   where
      p       = (:) <$> letterChar <*> many alphaNumChar
      check x =
         if x `elem` reservedWords
         then fail $ "keyword " ++ show x ++ " cannot be an identifier"
         else return x

-- Parsing
---------------------------------------------------------------------

parseType :: Parser Type
parseType = TyInt <$ reserved "int"
   <|> TyBool <$ reserved "boolean"
   <|> TyChar <$ reserved "char"

parseExprAtom :: Parser Expr
parseExprAtom = EInt <$> integer
   <|> (EBool True <$ reserved "true" <|> EBool False <$ reserved "false")
   <|> EChar <$> sQuotes L.charLiteral
   <|> EVar <$> identifier
   <|> parens parseExpr
 
parseExpr :: Parser Expr 
parseExpr = makeExprParser parseExprAtom operators 

operators :: [[Operator Parser Expr]]
operators = 
   [  [ Prefix (Un Neg <$ symbol "-")
      , Prefix (Un Not <$ symbol "!") ]

   ,  [ InfixL (Bin Mul <$ symbol "*")
      , InfixL (Bin Div <$ symbol "/") 
      , InfixL (Bin Mod <$ symbol "%") ]

   ,  [ InfixL (Bin Add <$ symbol "+")
      , InfixL (Bin Sub <$ symbol "-") ]

   ,  [ InfixL (Bin LessEq <$ reservedOp "<=") 
      , InfixL (Bin GreaterEq <$ reservedOp ">=" )
      , InfixL (Bin Less <$ symbol "<") 
      , InfixL (Bin Greater <$ symbol ">")  ]

   ,  [ InfixL (Bin Eq <$ reservedOp "==") 
      , InfixL (Bin NEq <$ reservedOp "!=") ]

   ,  [ InfixL (Bin And <$ reservedOp "&&") ]

   ,  [ InfixL (Bin Or <$ reservedOp "||") ]

   ]

parseStmt :: Parser Stmt
parseStmt = Output <$> (reserved "System" *> dot
      *> reserved "out" *> dot
      *> reserved "println" *> parens parseExpr)
   <|> try (DAndA <$> parseType <*> identifier <*> (reservedOp "=" *> parseExpr))
   <|> Assign <$> identifier <*> (reservedOp "=" *> parseExpr)
   <|> Declare <$> parseType <*> identifier

parseProg :: Parser Prog
parseProg = endBy parseStmt semi

parseDoc :: Parser Prog
parseDoc = between scn eof parseProg

oParse :: Parser a -> String -> Either (ParseErrorBundle String Void) a
oParse p = parse p ""

javaParse :: String -> Either (ParseErrorBundle String Void) Prog
javaParse = oParse parseDoc

checkJavaParse :: FilePath -> IO ()
checkJavaParse file = do
   s <- readFile file
   case javaParse s of
      Left err -> print err
      Right p  -> print p


-- Pretty-Printing:
---------------------------------------------------------------------

-- Will be used to add def main(): to beginning and main() and such.
printJava :: Prog -> String 
printJava = prettyJava ""

prettyJava :: String -> Prog -> String
prettyJava = foldl (\ text x -> text ++ printStmt x ++ ";\n")

printStmt :: Stmt -> String
printStmt (Assign var e)     = var ++ " = " ++ printExpr e
printStmt (Declare ty var)   = printType ty ++ " " ++ var
printStmt (DAndA ty var e)   = printType ty ++ " " ++ var ++ " = " ++ printExpr e
printStmt (Output e)         = "System.out.println(" ++ printExpr e ++ ")"

printType :: Type -> String
printType TyInt    = "int"
printType TyBool   = "boolean"
printType TyChar   = "char"
printType Poly     = "<Type unknown>"
printType (TVar t) = "Type variable: " ++ show t 

printExpr :: Expr -> String
printExpr (EInt n)      = show n
printExpr (EBool True)  = "true"
printExpr (EBool False) = "false"
printExpr (EChar c)     = show c
printExpr (EVar v)      = v
printExpr (Bin bop x y) = printExpr x ++ printBop bop ++ printExpr y
printExpr (Un uop x)    = printUop uop ++ printExpr x

printBop :: BOp -> String 
printBop Add = " + "
printBop Sub = " - "
printBop Mul = " * "
printBop Div = " / "
printBop Mod = " % "
printBop And = " && "
printBop Or  = " || "
printBop Eq  = " == "
printBop NEq = " != "
printBop Less = " < "
printBop Greater = " > "
printBop LessEq = " <= "
printBop GreaterEq = " >= "

printUop :: UOp -> String 
printUop Not = "!"
printUop Neg = "-"

