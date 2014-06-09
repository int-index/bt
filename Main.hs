{-# LANGUAGE LambdaCase #-}
module Main where

import System.Console.Haskeline hiding (complete)
import qualified Data.Map as M
import Control.Applicative hiding (Const)

import Expression
import Parser (parse)
import Render ()

main :: IO ()
main = runInputT settings (hello >> loop talk bye M.empty) where
    settings = Settings noCompletion Nothing True

data Step a
    = End
    | Keep
    | Update a

loop :: Monad m => (a -> m (Step a)) -> m b -> a -> m b
loop step end = next where
    next state = step state >>= \case
        End  -> end
        Keep -> next state
        Update state' -> next state'

hello :: InputT IO ()
hello = outputStrLn "The Bool Tool"

bye :: InputT IO ()
bye = outputStrLn "Bye!"

type Definitions = M.Map String Function

data Command
    = PassCommand
    | QuitCommand
    | HelpCommand
    | DefineCommand   String
    | UndefineCommand String
    | ShowCommand (Function -> Function) String
    | CompareCommand String String
    | ClassCommand String
    | CompleteCommand [String]
    | ListCommand
    | CleanCommand
    | PredefCommand

talk :: Definitions -> InputT IO (Step Definitions)
talk defs = getInputLine ">> " >>= \case
    Nothing -> return End
    Just commandString -> case parseCommand commandString of
        Nothing -> do
            outputStrLn "Couldn't parse the command..."
            return Keep
        Just command -> case command of
            QuitCommand -> return End
            PassCommand -> return Keep
            HelpCommand -> do
                outputStrLn helpMessage
                return Keep
            DefineCommand   name -> handleDefine   name defs
            UndefineCommand name -> handleUndefine name defs
            ShowCommand form name -> do
                handleShow form name defs
                return Keep
            CompareCommand name1 name2 -> do
                handleCompare name1 name2 defs
                return Keep
            ClassCommand name -> do
                handleClass name defs
                return Keep
            CompleteCommand names -> do
                handleComplete names defs
                return Keep
            ListCommand -> do
                handleList defs
                return Keep
            CleanCommand -> do
                return (Update $ M.empty)
            PredefCommand -> do
                return (Update $ M.union predef defs)

predef :: Definitions
predef = M.fromList
    [ ("1"   , function $ Const True)
    , ("0"   , function $ Const False)
    , ("id"  , function $ Access "x")
    , ("not" , function $ Not  (Access "x"))
    , ("and" , function $ And  (Access "x") (Access "y"))
    , ("or"  , function $ Or   (Access "x") (Access "y"))
    , ("xor" , function $ Xor  (Access "x") (Access "y"))
    , ("nand", function $ Nand (Access "x") (Access "y"))
    , ("nor" , function $ Nor  (Access "x") (Access "y"))
    , ("ent" , function $ Ent  (Access "x") (Access "y"))
    , ("equ" , function $ Equ  (Access "x") (Access "y"))
    ]

parseCommand :: String -> Maybe Command
parseCommand s = case words s of
    [] -> Just PassCommand
    ["quit"] -> Just QuitCommand
    ["help"] -> Just HelpCommand
    ["define",   name] -> Just (DefineCommand   name)
    ["undefine", name] -> Just (UndefineCommand name)
    ["show",     name] -> Just (ShowCommand id  name)
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
    ["predef"] -> Just PredefCommand
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
    , "predef            -- define standard functions"
    ]

handleDefine :: String -> Definitions -> InputT IO (Step Definitions)
handleDefine name defs = getInputLine (name ++ " = ") >>= \case
    Nothing -> return End
    Just s -> case parse s of
        Nothing -> do
            outputStrLn "Couldn't parse the expression..."
            return Keep
        Just fun -> do
            outputStrLn "Done!"
            return (Update $ M.insert name fun defs)

handleUndefine :: String -> Definitions -> InputT IO (Step Definitions)
handleUndefine name defs
    | M.member name defs = do
        outputStrLn "Done!"
        return (Update $ M.delete name defs)
    | otherwise = do
        notfound
        return Keep

handleList :: Definitions -> InputT IO ()
handleList defs = outputStrLn $ unlines (M.keys defs)

handleShow :: (Function -> Function) -> String -> Definitions -> InputT IO ()
handleShow form name defs = case M.lookup name defs of
    Just fun -> outputStrLn (show $ form fun)
    Nothing  -> notfound

handleCompare :: String -> String -> Definitions -> InputT IO ()
handleCompare name1 name2 defs
    | name1 == name2 = outputStrLn "You're kidding, right?"
    | otherwise = case (,) <$> M.lookup name1 defs <*> M.lookup name2 defs of
        Just (fun1, fun2) -> if fun1 == fun2
            then outputStrLn "Equal"
            else outputStrLn "Different"
        Nothing -> notfound

handleClass :: String -> Definitions -> InputT IO ()
handleClass name defs = case M.lookup name defs of
    Just fun -> outputStrLn $ unwords $ map show (postClasses fun)
    Nothing  -> notfound

handleComplete :: [String] -> Definitions -> InputT IO ()
handleComplete names defs = case mapM (flip M.lookup defs) names of
    Just funs -> if complete funs
        then outputStrLn "Complete"
        else outputStrLn "Incomplete"
    Nothing -> notfound

notfound :: InputT IO ()
notfound = outputStrLn "Not found"
