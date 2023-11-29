

module Main where




import            Omni.Data
import            Omni.Python.Syntax         (pyParse, checkPyParse)
import            Omni.Python.Pretty         (printPython) 
import            Omni.Java.Syntax           (javaParse, checkJavaParse)
import            Omni.Java.Pretty           (printJava)

import            Omni.Typecheck.Data    
import            Omni.Typecheck.Elaborate   (checkProg)
import            System.Console.Haskeline
import qualified  Data.Map                   as M
import            Control.Monad.State 
import            Control.Monad.Reader 
import            Control.Monad.Writer 
import            Control.Monad.Except
import            Control.Monad.Identity


-- Python:
    -- https://docs.python.org/3/reference/grammar.html
    -- https://www.w3schools.com/python/python_syntax.asp
    -- https://www.w3schools.com/python/python_ref_keywords.asp

-- MegaParsec:
    -- https://hackage.haskell.org/package/megaparsec
    -- https://hackage.haskell.org/package/megaparsec-9.5.0#megaparsec-vs-parsec
    -- https://github.com/mrkkrp/megaparsec-site/blob/master/tutorials/parsing-simple-imperative-language.md
    -- https://github.com/disco-lang/disco/blob/master/src/Disco/Parser.hs


data OmniState = QS
   { omniDoc  :: Maybe (IO String)
   , omniSrc  :: Maybe String
   }

type OmniM = StateT OmniState (InputT IO)

description :: String
description = "\nWelcome to Omni:\n\n\
   \  Omni is a program for transcribing programming language files.\n\
   \  Omni currently only supports Python and Java source code.\n\n\
   \Commands:\n\
   \  :toPyth [file name]     -- Transcribe a program to Python\n\
   \  :toJava [file name]     -- Transcribe a program to Java\n\
   \  :quit                   -- Exit\n\
   \Currently, input files must be located in the `TestFiles` directory.\n"

omniSettings :: Settings IO
omniSettings = defaultSettings
   { historyFile = Just "omni_history.txt" }

omniREPL :: IO ()
omniREPL = do
   putStrLn description
   runInputT omniSettings . flip evalStateT (QS Nothing Nothing) $ loop
   where
      loop :: OmniM ()
      loop = do
         minput <- lift $ getInputLine "> "
         case minput of
               Nothing      -> return ()
               Just s       -> do
                  let (cmd:rest) = words s
                  shouldLoop <- case cmd of
                     ":q"        -> return False
                     ":quit"     -> return False
                     x           -> 
                        if null rest || length rest > 1
                        then lift (outputStrLn "Invalid input") >> return True
                        else let (f:rest') = rest
                        in case x of 
                           ":toPyth" -> toPy   f (readFile ("TestFiles/" ++ f)) >> return True
                           ":toJava" -> toJava f (readFile ("TestFiles/" ++ f)) >> return True
                           "p" -> toPy   f (readFile ("TestFiles/" ++ f)) >> return True
                           "j" -> toJava f (readFile ("TestFiles/" ++ f)) >> return True
                           ":parse"  ->
                              let (fName, ext) = stripExtension "" f
                              in liftIO $ parseCheck (extToLang ext) f          >> return True -- Will be removed at some point
                           ":large"  -> toTest (read f)                         >> return True
                           _           -> lift (outputStrLn ("Error: " ++ s))   >> return True
                  when shouldLoop loop

stripExtension :: String -> String -> (String, String)
stripExtension f []     = (f, [])
stripExtension f (x:xs) =
   case x of
      '.' -> (f, xs)
      _   -> stripExtension (f ++ [x]) xs

-- REMOVE TESTFILES FOLDER EXTENSION
pySave :: String -> String -> OmniM ()
pySave s doc = do
   liftIO $ maybe (return ()) (writeFile ("TestFiles/"++s ++ ".py")) (Just doc)
   lift $ outputStrLn ("The file has been successfully created: \
   \" ++ s ++ ".py")

-- REMOVE TESTFILES FOLDER EXTENSION
javaSave :: String -> String -> OmniM ()
javaSave s doc = do
   liftIO $ maybe (return ()) (writeFile ("TestFiles/"++s ++ ".java")) (Just doc)
   lift $ outputStrLn ("The file has been successfully created: \
   \" ++ s ++ ".java")

extToLang :: String -> Lang
extToLang x =
   case x of
      "py"   -> Python
      "java" -> Java
      y      -> error $ "`" ++ y ++ "` files are not supported."

toPy :: String -> IO String -> OmniM ()
toPy s txt =
   let (fName, ext) = stripExtension "" s
   in if ext == "py"
      then lift $ outputStrLn "The file is already a Python file."
      else do
         txt' <- liftIO txt
         case translate txt' s (extToLang ext) Python of
            Left  err -> error $ show err
            Right f   -> do
               modify (\qs -> qs { omniDoc = Just txt
                                 , omniSrc = Just s
                                 }
                     )
               pySave fName f

toJava :: String -> IO String -> OmniM ()
toJava s txt =
   let (fName, ext) = stripExtension "" s
   in if ext == "java"
      then lift $ outputStrLn "The file is already a Java file."
      else do
         txt' <- liftIO txt
         case translate txt' s (extToLang ext) Java of
            Left  err -> error $ show err
            Right f   -> do
               modify (\qs -> qs { omniDoc = Just txt
                                 , omniSrc = Just s
                                 }
                     )
               javaSave fName f

toTest :: Int -> OmniM ()
toTest i = 
   case testTranslate i "" "0" of
      Left  err -> error $ show err
      Right f   -> testSave f

testTranslate :: Int -> String -> String -> Either Error String 
testTranslate 0 str x = Right $ "def main():\n" ++ str 
testTranslate i str x = do 
   let x' = '[' : x ++ "]"
   let x'' = "\n\tx = " ++ x'
   testTranslate (i-1) (x'' ++ str) x'

testSave :: String -> OmniM ()
testSave f = do
   liftIO $ maybe (return ()) (writeFile "TestFiles/large.py") (Just f)
   lift $ outputStrLn "The file has been successfully created: \
   \large.py"

translate :: String -> String -> Lang -> Lang -> Either Error String
translate txt name lang new = do
   let (name',_) = stripExtension "" name
   parsedProg  <- parse lang txt name
   checkedProg <- typeCheck lang parsedProg 
   prettyProg  <- prettyPrint new name' checkedProg
   Right prettyProg

parse :: Lang -> String -> String -> Either Error Prog 
parse Python txt name = pyParse txt name 
parse Java   txt _    = javaParse txt

typeCheck :: Lang -> Prog -> Either Error Prog 
typeCheck Python p = 
   case checkProg p of 
      Left err -> Left $ TypeError err 
      Right p' -> Right p'
typeCheck _      p = Right p

prettyPrint :: Lang -> String -> Prog -> Either Error String
prettyPrint Python _    p = 
   case printPython p of 
      Left err -> Left $ PrettyError err
      Right p' -> Right p'
prettyPrint Java   name p = 
   case printJava name p of 
      Left err -> Left $ PrettyError err 
      Right p' -> Right p'

parseCheck ::  Lang -> FilePath -> IO ()
parseCheck lang =
   case lang of
      Python -> checkPyParse
      Java   -> checkJavaParse

main :: IO ()
main = omniREPL

