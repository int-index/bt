{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Main where

import System.Console.Haskeline hiding (complete)
import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Lens (declareLenses, (%=), use)
import Data.List (sortBy)
import Data.Bool
import Data.Monoid

import Boolean.Expression
import Boolean.Analysis
import Boolean.Operator
import Boolean.Parser (parse)
import Boolean.Render (rFunction)

import qualified Boolean.Predef as Predef

declareLenses [d|

    data UserState = UserState
        { definitions :: Definitions, operators :: Operators }

                |]

defaultUserState :: UserState
defaultUserState = UserState Predef.functions Predef.operators

emptyUserState :: UserState
emptyUserState = UserState M.empty M.empty

main :: IO ()
main = runInputT settings (evalStateT ui defaultUserState) where
    settings = Settings noCompletion Nothing True
    ui = hello >> loop talk bye

loop :: Monad m => m Bool -> m a -> m a
loop step end = fix (\go -> step >>= bool end go)

type M a = StateT UserState (InputT IO) a

withInputLine :: d -> String -> (String -> M d) -> M d
withInputLine d s a = lift (getInputLine s) >>= maybe (return d) a

outputLine :: String -> M ()
outputLine s = lift (outputStrLn s)

simply :: Monad m => m a -> m Bool
simply a = a >> return True

hello = outputLine "The Bool Tool"
bye   = outputLine "Bye!"


data Command
    = PassCommand
    | QuitCommand
    | HelpCommand
    | DefineCommand   String
    | UndefineCommand String
    | ShowCommand ShowForm String
    | CompareCommand String String
    | ClassCommand String
    | CompleteCommand [String]
    | ListCommand
    | CleanCommand
    | ResetCommand
    | OperatorsCommand

data ShowForm
    = ShowDefault
    | ShowCNF
    | ShowDNF
    | ShowANF
    | ShowTable

talk :: M Bool
talk = withInputLine False ">> " $ \commandString -> do
    case parseCommand commandString of
        Nothing -> simply $ outputLine "Couldn't parse the command..."
        Just command -> case command of
            QuitCommand -> return False
            PassCommand -> return True
            HelpCommand -> simply $ outputLine helpMessage
            DefineCommand    name -> simply $ handleDefine    name
            UndefineCommand  name -> simply $ handleUndefine  name
            ShowCommand form name -> simply $ handleShow form name
            CompareCommand name1 name2 -> simply $ handleCompare name1 name2
            ClassCommand name -> simply $ handleClass name
            CompleteCommand names -> simply $ handleComplete names
            ListCommand  -> simply $ use definitions >>= outputLine . unlines . M.keys
            CleanCommand -> simply $ put   emptyUserState
            ResetCommand -> simply $ put defaultUserState
            OperatorsCommand -> simply $ handleOperators

parseCommand :: String -> Maybe Command
parseCommand s = case words s of
    [] -> Just PassCommand
    ["quit"] -> Just QuitCommand
    ["help"] -> Just HelpCommand
    ["define",   name] -> Just (DefineCommand   name)
    ["undefine", name] -> Just (UndefineCommand name)
    ["show",     name] -> Just (ShowCommand ShowDefault name)
    ["show", form, name] -> case form of
       "cnf"   -> Just (ShowCommand ShowCNF   name)
       "dnf"   -> Just (ShowCommand ShowDNF   name)
       "anf"   -> Just (ShowCommand ShowANF   name)
       "table" -> Just (ShowCommand ShowTable name)
       _ -> Nothing
    ["compare", name1, name2] -> Just (CompareCommand name1 name2)
    ["class",   name]  -> Just (ClassCommand name)
    ("complete":names) -> Just (CompleteCommand names)
    ["list"]  -> Just ListCommand
    ["clean"] -> Just CleanCommand
    ["reset"] -> Just ResetCommand
    ["operators"] -> Just OperatorsCommand
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

handleDefine :: String -> M ()
handleDefine name = withInputLine () (name ++ " = ") $ \s -> do
    ops <- use operators
    case parse ops s of
        Left  msg -> do outputLine "Couldn't parse the expression..."
                        outputLine (show msg)
        Right fun -> do definitions %= M.insert name fun
                        outputLine "Done!"

handleUndefine :: String -> M ()
handleUndefine name = do
    definitions %= M.delete name
    outputLine "Done!"

handleShow :: ShowForm -> String -> M ()
handleShow form name = do
    ops  <- use operators
    defs <- use definitions
    runEvaluate defs
        (outputLine . show)
        (outputLine . rFunction ops)
        (dispatch form `onFunction` name)
    where dispatch form = case form of
              ShowDefault -> return
              ShowCNF     -> conjunctive nf
              ShowDNF     -> disjunctive nf
              ShowANF     -> algebraic nf
              ShowTable   -> tablify

handleCompare :: String -> String -> M ()
handleCompare name1 name2
    | name1 == name2 = outputLine "You're kidding, right?"
    | otherwise = do
        defs <- use definitions
        runEvaluate defs
            (outputLine . show)
            (outputLine . bool "Different" "Equal")
            (liftL2 onFunction funeq name1 name2)

handleClass :: String -> M ()
handleClass name = do
    defs <- use definitions
    runEvaluate defs
        (outputLine . show)
        (outputLine . unwords . map show)
        (postClasses `onFunction` name)

handleComplete :: [String] -> M ()
handleComplete names = do
    defs <- use definitions
    runEvaluate defs
        (outputLine . show)
        (outputLine . bool "Incomplete" "Complete")
        (mapM (onFunction return) names >>= complete)

handleOperators :: M ()
handleOperators = do
    ops <- use operators
    let opNameLength = maximum (map length $ M.keys ops)
        aliases = \case
            NullaryOperator as   -> as
            UnaryOperator   as _ -> as
            BinaryOperator  as _ -> as
        fixshow = \case
            NullaryOperator _ -> "nullfix "
            UnaryOperator  _ (_, fixity) -> case fixity of
                Prefix   -> "prefix  "
                Postfix  -> "postfix "
            BinaryOperator _ (_, fixity) -> case fixity of
                Leftfix  -> "leftfix "
                Rightfix -> "rightfix"
                Nonfix   -> "nonfix  "
        align maxLength s = s ++ replicate (maxLength - length s) ' '
        -- a rather hacky formatting function
        format (name, op) = unwords $
            [ align (opNameLength + 1) name
            , align 9 (fixshow op)
            ] ++ map (align 3) (aliases op)
    mapM_ (outputLine . format) (sortOps ops)

sortOps :: Operators -> [(String, Operator)]
sortOps = sortBy cmp . M.toList where
    value = \case
        NullaryOperator _ -> Nothing
        UnaryOperator   _ (i, _) -> Just i
        BinaryOperator  _ (i, _) -> Just i
    cmp (name1, op1) (name2, op2) = compare (value op1) (value op2)
                                 <> compare name1 name2
