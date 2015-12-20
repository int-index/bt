{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Data.String (fromString)
import Control.Monad
import Control.Applicative
import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Parser.Combinators
import qualified Text.Parsec as Parsec

import qualified Boolean.Operator as O
import Boolean.Parser (pFunction)

import Command

parse p = either (const Nothing) Just . Parsec.parse (whiteSpace *> p <* eof) ""

pCommand :: O.Operators -> Parsec.Parsec String () Command
pCommand ops = pDefine <|> pTextCmd <|> pPass where

    pDefine = do
        name <- ident emptyIdents
        symbolic '='
        DefineCommand (fromString name) <$> pFunction ops
            <|> return (UndefineCommand (fromString name))

    pTextCmd = do
        symbolic ':'
        some (ident emptyIdents) >>= \case
            ["quit"] -> return QuitCommand
            ["help"] -> return HelpCommand
            ["list"] -> return ListCommand
            ["show", name] -> return (ShowCommand ShowDefault (fromString name))
            ["show", form, name] -> case form of
               "cnf"   -> return (ShowCommand ShowCNF   (fromString name))
               "dnf"   -> return (ShowCommand ShowDNF   (fromString name))
               "anf"   -> return (ShowCommand ShowANF   (fromString name))
               "table" -> return (ShowCommand ShowTable (fromString name))
               _ -> mzero
            ["class",   name]  -> return (ClassCommand (fromString name))
            ("complete":names) -> return (CompleteCommand (map fromString names))
            ["clean"] -> return CleanCommand
            ["reset"] -> return ResetCommand
            ["operators"] -> return OperatorsCommand
            _ -> mzero

    pPass = PassCommand <$ eof

pEvalCommand ops = EvalCommand <$> pFunction ops

parseCommand :: O.Operators -> String -> Maybe Command
parseCommand ops s = parse (pEvalCommand ops) s <|> parse (pCommand ops) s
