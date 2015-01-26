{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Main where

import System.Console.Haskeline hiding (complete)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Lens (declareLenses, (%=), use, uses)
import Data.Char (isSpace)
import Data.List (sortBy)
import Data.Bool
import Data.Monoid

import Boolean.Expression
import Boolean.Analysis
import Boolean.Operator
import Boolean.Render (rFunction)

import qualified Boolean.Predef as Predef

import Command
import Parser

declareLenses [d|

    data UserState = UserState
        { definitions :: Definitions, operators :: Operators }

                |]

defaultUserState :: UserState
defaultUserState = UserState Predef.functions Predef.operators

emptyUserState :: UserState
emptyUserState = UserState M.empty M.empty

main :: IO ()
main = runInputT settings (evalStateT ui defaultUserState)
  where
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

talk :: M Bool
talk = withInputLine False ">> " $ \commandString -> do
    ops <- use operators
    case parseCommand ops commandString of
        Nothing -> simply $ outputLine "Couldn't parse the command..."
        Just QuitCommand -> return False
        Just command -> simply $ case command of
            PassCommand -> return ()
            HelpCommand -> outputLine helpMessage
            DefineCommand name fun -> handleDefine name fun
            UndefineCommand  name -> handleUndefine  name
            ShowCommand form name -> handleShow form name
            ClassCommand name -> handleClass name
            CompleteCommand names -> handleComplete names
            ListCommand  -> use definitions >>= outputLine . unlines . M.keys
            CleanCommand -> put   emptyUserState
            ResetCommand -> put defaultUserState
            OperatorsCommand -> handleOperators
            EvalCommand fun  -> handleEval fun

helpMessage :: String
helpMessage = unlines
    [ "Type one of the following commands:"
    , ":help              -- display this message"
    , ":quit              -- quit the program"
    , "EXPR               -- evaluate an expression"
    , "NAME = EXPR        -- define a function"
    , "NAME =             -- undefine a function"
    , ":show      NAME    -- show a function"
    , ":show FORM NAME    -- show a function in a specific form"
    , "      forms: cnf, dnf, anf, table"
    , ":class     NAME    -- show classes of a function"
    , ":complete [NAME]   -- check whether a function system is complete"
    , ":list              -- list defined functions"
    , ":clean             -- undefine everything"
    , ":reset             -- define standard functions"
    ]

handleDefine :: String -> Function -> M ()
handleDefine name fun = do
    handleAssuming fun
    overwriting <- uses definitions (M.member name)
    if not overwriting then commit else do
        outputLine "Overwrite existing definition?"
        withInputLine () "(empty line if yes) " $ \str -> do
            when (all isSpace str) commit
  where
    commit = definitions %= M.insert name fun

handleUndefine :: String -> M ()
handleUndefine name = do
    definitions %= M.delete name

infixl 0 `runEval`

runEval :: Evaluate a -> (a -> M ()) -> M ()
runEval x action = do
    defs <- use definitions
    runEvaluate defs (outputLine . show) action x

handleShow :: ShowForm -> String -> M ()
handleShow form name = do
    ops <- use operators
    dispatch form `onFunction` name
        `runEval` \fun -> outputLine (rFunction ops fun)
  where
    dispatch form = case form of
        ShowDefault -> return
        ShowCNF     -> conjunctive nf
        ShowDNF     -> disjunctive nf
        ShowANF     -> algebraic nf
        ShowTable   -> tablify

handleClass :: String -> M ()
handleClass name = do
    postClasses `onFunction` name
        `runEval` \x -> outputLine (unwords . map show $ S.toList x)

handleComplete :: [String] -> M ()
handleComplete names = do
    mapM (onFunction return) names >>= complete
        `runEval` \p -> outputLine (if p then "Complete" else "Incomplete")

handleAssuming :: Function -> M ()
handleAssuming = \case
    Function params _ -> outputLine ("Assuming: λ " ++ unwords params)
    _ -> return ()

handleEval :: Function -> M ()
handleEval fun = do
    handleAssuming fun
    test fun `runEval` \p -> outputLine (if p then "True" else "False")

handleOperators :: M ()
handleOperators = do
    ops <- use operators
    let opNameLength = maximum (map length $ M.keys ops)
        aliases = \case
            NullaryOperator as   -> as
            UnaryOperator   as _ -> as
            BinaryOperator  as _ -> as
        fixshow = \case
            NullaryOperator _ -> "nullfix"
            UnaryOperator  _ (_, fixity) -> case fixity of
                Prefix   -> "prefix"
                Postfix  -> "postfix"
            BinaryOperator _ (_, fixity) -> case fixity of
                Leftfix  -> "leftfix"
                Rightfix -> "rightfix"
                Nonfix   -> "nonfix"
        align maxLength s = s ++ replicate (maxLength - length s) ' '
        -- a rather hacky formatting function
        format (name, op) = unwords $
            [ align (opNameLength + 1) name
            , align 9 (fixshow op)
            ] ++ map (align 3) (aliases op)
    mapM_ (outputLine . format) (sortOps ops)

sortOps :: Operators -> [(String, Operator)]
sortOps = sortBy cmp . M.toList
  where
    value = \case
        NullaryOperator _ -> Nothing
        UnaryOperator   _ (i, _) -> Just i
        BinaryOperator  _ (i, _) -> Just i
    cmp (name1, op1) (name2, op2) = compare (value op1) (value op2)
                                 <> compare name1 name2
