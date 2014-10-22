{-# LANGUAGE LambdaCase #-}
module Parser where

import Control.Monad
import Control.Applicative
import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Parser.Combinators
import qualified Text.Parsec as Parsec

import qualified Boolean.Operator   as O
import Boolean.Parser (pFunction)

import Command

parse p = either (const Nothing) Just . Parsec.parse (whiteSpace *> p <* eof) ""

pCommand :: O.Operators -> Parsec.Parsec String () Command
pCommand ops = pDefine <|> pTextCmd <|> pPass where

    pName  = ident emptyIdents

    pDefine = do
        name <- pName
        symbolic '='
        DefineCommand name <$> pFunction ops
            <|> return (UndefineCommand name)

    pTextCmd = do
        symbolic ':'
        some pName >>= \case
            ["quit"] -> return QuitCommand
            ["help"] -> return HelpCommand
            ["list"] -> return ListCommand
            ["show", name] -> return (ShowCommand ShowDefault name)
            ["show", form, name] -> case form of
               "cnf"   -> return (ShowCommand ShowCNF   name)
               "dnf"   -> return (ShowCommand ShowDNF   name)
               "anf"   -> return (ShowCommand ShowANF   name)
               "table" -> return (ShowCommand ShowTable name)
               _ -> mzero
            ["class",   name]  -> return (ClassCommand name)
            ("complete":names) -> return (CompleteCommand names)
            ["clean"] -> return CleanCommand
            ["reset"] -> return ResetCommand
            ["operators"] -> return OperatorsCommand
            _ -> mzero

    pPass = PassCommand <$ eof

pEvalCommand ops = EvalCommand <$> pFunction ops

parseCommand :: O.Operators -> String -> Maybe Command
parseCommand ops s = parse (pEvalCommand ops) s <|> parse (pCommand ops) s
