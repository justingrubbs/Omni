

module Main where


import            Omni.Data
import            Omni.Python.Syntax         (pyParse, checkPyParse)
import            Omni.Python.Pretty         (printPython) 
import            Omni.Java.Syntax           (javaParse, checkJavaParse)
import            Omni.Java.Pretty           (printJava)
import            Omni.Typestuff             (progEdit)
import            Omni.Typecheck.Monadic     (checkProg, Contexts)
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
   \  :quit                   -- Exit\n"

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
                        -- Was just using (cmd:r:rest) but this caused error message when `:q` or `:quit`
                        -- However, doing this at all allows weird stuff like:
                           -- `:parse javaTest.java javaTest.java ldfjkls dfofnic`
                        let (r:rest') = rest
                        in case x of
                           ":toPyth" -> toPy   r (readFile r)         >> return True
                           ":toJava" -> toJava r (readFile r)         >> return True
                           ":parse"  ->
                              let (fName, ext) = stripExtension "" r
                              in liftIO $ parseCheck (extToLang ext) r  >> return True -- Will be removed at some point
                           _           -> lift (outputStrLn ("Error: " ++ s))       >> return True
                  when shouldLoop loop

stripExtension :: String -> String -> (String, String)
stripExtension f []     = (f, [])
stripExtension f (x:xs) =
   case x of
      '.' -> (f, xs)
      _   -> stripExtension (f ++ [x]) xs

pySave :: String -> String -> OmniM ()
pySave s doc = do
   liftIO $ maybe (return ()) (writeFile (s ++ ".py")) (Just doc)
   lift $ outputStrLn ("The file has been successfully created: \
   \" ++ s ++ ".py")

javaSave :: String -> String -> OmniM ()
javaSave s doc = do
   liftIO $ maybe (return ()) (writeFile (s ++ ".java")) (Just doc)
   lift $ outputStrLn ("The file has been successfully created: \
   \" ++ s ++ ".java")

extToLang :: String -> Lang
extToLang x =
   case x of
      "py"   -> Python
      "java" -> Java
      y      -> error ("Extension " ++ y ++ " is not supported.")

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
         -- case translate txt' (extToLang ext) Java of
            Left  err -> error $ show err
            Right f   -> do
               modify (\qs -> qs { omniDoc = Just txt
                                 , omniSrc = Just s
                                 }
                     )
               javaSave fName f

-- The lefts of translate and typecheck should be specific errors but whatever for now -- probably elsewhere as well
-- STILL NEED TO DO SOME TYPE CHECKING FOR JAVA? JUST FOR CONSISTENCY? UNDEFINED VARS AND SUCH  -- no
translate :: String -> String -> Lang -> Lang -> Either Error String
translate txt name lang new = do
   let (name',_) = stripExtension "" name
   case lang of 
      -- This can all be generalized much better
      Python -> 
         case pyParse txt name of
            Left err -> Left $ ParseError $ Generic err
            Right p  -> do
               -- Change `checkProg` to `progEdit` to use old type checker
               case checkProg p of           
                  Left err -> Left $ TypeError err
                  Right p' -> 
                     case prettyPrint new name' p' of 
                        Left err -> Left $ PrettyError err
                        Right x  -> Right x
      Java -> 
         case javaParse txt of
            Left err -> Left $ ParseError $ Generic err
            Right p  -> 
               case prettyPrint new name' p of 
                  Left err -> Left $ PrettyError err
                  Right x  -> Right x

prettyPrint :: Lang -> String -> Prog -> Either PrettyError String
prettyPrint Python _    = printPython
prettyPrint Java   name = printJava name

parseCheck ::  Lang -> FilePath -> IO ()
parseCheck lang =
   case lang of
      Python -> checkPyParse
      Java   -> checkJavaParse

main :: IO ()
main = omniREPL

