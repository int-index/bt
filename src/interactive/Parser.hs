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
pCommand ops = pCmd1 <|> pList <|> pTextCmd <|> pPass where

    pName  = ident emptyIdents

    pDefine'  name  = DefineCommand name <$> pFunction ops
    pCompare' name1 = do
        name2 <- pName
        return (CompareCommand name1 name2)

    pCmd1 = do
        name <- pName
        Parsec.char '='
        try (Parsec.char '=') *> whiteSpace *> pCompare' name
            <|> whiteSpace *> (pDefine' name <|> return (UndefineCommand name))

    pList = symbol ".." >> return ListCommand

    pTextCmd = do
        symbolic ':'
        some pName >>= \case
            ["quit"] -> return QuitCommand
            ["help"] -> return HelpCommand
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
