module Main where


import Omni.Data
import Omni.Python                  (pyParse, checkPyParse, printPython)
import Omni.Java                    (javaParse, checkJavaParse, printJava)
import Omni.Typecheck               (progEdit)
import Control.Monad.State
import System.Console.Haskeline
import qualified Data.Map as        M


-- https://hackage.haskell.org/package/pandoc

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
   \  Omni currently only supports Python and Java files.\n\n\
   \Commands:\n\
   \  :toPython [file name]   -- Transcribe a program to Python\n\
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
                           -- `:parse javaTest.jav javaTest.jav ldfjkls dfofnic`
                        let (r:rest') = rest
                        in case x of
                           ":toPython" -> toPy   r (readFile r)         >> return True
                           ":toJava"   -> toJava r (readFile r)         >> return True
                           ":parse"    ->
                              let (fName, ext) = stripExtension "" r
                              in liftIO $ parseCheck (extToLang ext) r  >> return True  -- Will be removed at some point
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
   liftIO $ maybe (return ()) (writeFile (s ++ ".jav")) (Just doc)
   lift $ outputStrLn ("The file has been successfully created: \
   \" ++ s ++ ".jav")

extToLang :: String -> Lang
extToLang x =
   case x of
      "py"   -> Python
      "jav"  -> Java
      y      -> error ("Extension " ++ y ++ " is not supported.")

toPy :: String -> IO String -> OmniM ()
toPy s txt =
   let (fName, ext) = stripExtension "" s
   in if ext == "py"
      then lift $ outputStrLn "The file is already a Python file."
      else do
         txt' <- liftIO txt
         case translate txt' s (extToLang ext) Python of
            Left  err -> lift $ outputStrLn err
            Right f   -> do
               modify (\qs -> qs { omniDoc = Just txt
                                 , omniSrc = Just s
                                 }
                     )
               pySave fName f

toJava :: String -> IO String -> OmniM ()
toJava s txt =
   let (fName, ext) = stripExtension "" s
   in if ext == "jav"
      then lift $ outputStrLn "The file is already a Java file."
      else do
         txt' <- liftIO txt
         case translate txt' s (extToLang ext) Java of
         -- case translate txt' (extToLang ext) Java of
            Left  err -> lift $ outputStrLn err
            Right f   -> do
               modify (\qs -> qs { omniDoc = Just txt
                                 , omniSrc = Just s
                                 }
                     )
               javaSave fName f

-- The lefts of translate and typecheck should be specific errors but whatever for now -- probably elsewhere as well
-- STILL NEED TO DO SOME TYPE CHECKING FOR JAVA? JUST FOR CONSISTENCY? UNDEFINED VARS AND SUCH
translate :: String -> String -> Lang -> Lang -> Either String String
translate txt name Python new =
   case pyParse txt name of
      Left err -> Left $ show err
      Right p  -> do
         p' <- typeCheck p
         Right (prettyPrint new p')
translate txt _ Java   new =
   case javaParse txt of
      Left err -> Left $ show err
      Right p  -> Right (prettyPrint new p)

typeCheck :: Prog -> Either String Prog
typeCheck p =
   case progEdit p of
      Left err -> Left $ show err
      Right x  -> Right x

prettyPrint :: Lang -> Prog -> String
prettyPrint Python = printPython
prettyPrint Java   = printJava

parseCheck ::  Lang -> FilePath -> IO ()
parseCheck lang =
   case lang of
      Python -> checkPyParse
      Java   -> checkJavaParse

main :: IO ()
main = omniREPL

