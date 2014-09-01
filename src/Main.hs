{-# LANGUAGE LambdaCase #-}
module Main where

import System.Console.Haskeline hiding (complete)
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.State.Strict
import Data.Bool

import Expression
import Parser (parse)
import Render ()

main :: IO ()
main = runInputT settings (evalStateT (hello >> loop talk bye) predef) where
    settings = Settings noCompletion Nothing True

loop :: Monad m => m Bool -> m a -> m a
loop step end = go
    where go = step >>= bool end go

type M a = StateT Definitions (InputT IO) a

withInputLine :: String -> (String -> M Bool) -> M Bool
withInputLine s a = lift (getInputLine s) >>= maybe (return False) a

outputLine :: String -> M ()
outputLine s = lift (outputStrLn s)

simply :: Monad m => m a -> m Bool
simply a = a >> return True

hello    = outputLine "The Bool Tool"
bye      = outputLine "Bye!"
notfound = outputLine "Not found"


data Command
    = PassCommand
    | QuitCommand
    | HelpCommand
    | DefineCommand   String
    | UndefineCommand String
    | ShowCommand (Definitions -> Function -> Function) String
    | CompareCommand String String
    | ClassCommand String
    | CompleteCommand [String]
    | ListCommand
    | CleanCommand
    | ResetCommand


talk :: M Bool
talk = withInputLine ">> " $ \commandString -> do
    case parseCommand commandString of
        Nothing -> simply $ outputLine "Couldn't parse the command..."
        Just command -> case command of
            QuitCommand -> return False
            PassCommand -> return True
            HelpCommand -> simply $ outputLine helpMessage
            DefineCommand    name -> handleDefine    name
            UndefineCommand  name -> simply $ handleUndefine  name
            ShowCommand form name -> simply $ handleShow form name
            CompareCommand name1 name2 -> simply $ handleCompare name1 name2
            ClassCommand name -> simply $ handleClass name
            CompleteCommand names -> simply $ handleComplete names
            ListCommand  -> simply $ get >>= outputLine . unlines . M.keys
            CleanCommand -> simply $ put M.empty
            ResetCommand -> simply $ put predef

parseCommand :: String -> Maybe Command
parseCommand s = case words s of
    [] -> Just PassCommand
    ["quit"] -> Just QuitCommand
    ["help"] -> Just HelpCommand
    ["define",   name] -> Just (DefineCommand   name)
    ["undefine", name] -> Just (UndefineCommand name)
    ["show",     name] -> Just (ShowCommand (const id) name)
    ["show", form, name] -> case form of
       "cnf"   -> Just (ShowCommand (conjunctive nf) name)
       "dnf"   -> Just (ShowCommand (disjunctive nf) name)
       "anf"   -> Just (ShowCommand anf name)
       "table" -> Just (ShowCommand tablify name)
       _ -> Nothing
    ["compare", name1, name2] -> Just (CompareCommand name1 name2)
    ["class",   name]  -> Just (ClassCommand name)
    ("complete":names) -> Just (CompleteCommand names)
    ["list"]  -> Just ListCommand
    ["clean"] -> Just CleanCommand
    ["reset"] -> Just ResetCommand
    _ -> Nothing

helpMessage :: String
helpMessage = unlines
    [ "Type one of the following commands:"
    , "help              -- display this message"
    , "quit              -- quit the program"
    , "define    NAME    -- define a function"
    , "undefine  NAME    -- undefine a function"
    , "show      NAME    -- show a function"
    , "show FORM NAME    -- show a function in a specific form"
    , "     forms: cnf, dnf, anf, table"
    , "compare NAME NAME -- compare two functions"
    , "class     NAME    -- show classes of a function"
    , "complete [NAME]   -- check whether a function system is complete"
    , "list              -- list defined functions"
    , "clean             -- undefine everything"
    , "reset             -- define standard functions"
    ]

handleDefine :: String -> M Bool
handleDefine name = withInputLine (name ++ " = ") $ \s ->
    simply $ case parse s of
        Nothing  -> outputLine "Couldn't parse the expression..."
        Just fun -> do modify $ M.insert name fun
                       outputLine "Done!"

handleUndefine :: String -> M ()
handleUndefine name = get >>= \defs -> bool notfound
    (modify (M.delete name) >> outputLine "Done!")
    (M.member name defs)

handleShow :: (Definitions -> Function -> Function) -> String -> M ()
handleShow form name = get >>= \defs -> maybe notfound
    (outputLine . show . form defs) (M.lookup name defs)

handleCompare :: String -> String -> M ()
handleCompare name1 name2
    | name1 == name2 = outputLine "You're kidding, right?"
    | otherwise = get >>= \defs ->
        let eq = funeq defs <$> M.lookup name1 defs <*> M.lookup name2 defs
        in maybe notfound (outputLine . bool "Different" "Equal") eq

handleClass :: String -> M ()
handleClass name = get >>= \defs -> maybe notfound
    (outputLine . unwords . map show . postClasses defs)
    (M.lookup name defs)

handleComplete :: [String] -> M ()
handleComplete names = get >>= \defs -> maybe notfound
    (outputLine . bool "Incomplete" "Complete" . complete defs)
    (forM names $ \name -> M.lookup name defs)
