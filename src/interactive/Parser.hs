module Parser where

import Control.Applicative
import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Parser.Combinators
import qualified Text.Parsec as Parsec

import qualified Boolean.Operator   as O
import Boolean.Parser (pFunction)

import Command

parse p = Parsec.parse (whiteSpace *> p <* eof) ""

pCommand :: O.Operators -> Parsec.Parsec String () Command
pCommand ops = pCmd1 <|> pList where

    pName  = ident emptyIdents

    pDefine'  name  = DefineCommand name <$> pFunction ops
    pCompare' name1 = do
        name2 <- pName
        return (CompareCommand name1 name2)

    pCmd1 = do
        name <- pName
        Parsec.string "="
        try (Parsec.string "=") *> whiteSpace *> pCompare' name
            <|> whiteSpace *> (pDefine' name <|> return (UndefineCommand name))

    pList = do
        Parsec.string ".."
        whiteSpace
        return ListCommand

parseCommand :: O.Operators -> String -> Maybe Command
parseCommand ops s =
    case parse (pCommand ops) s of
        Right cmd -> Just cmd
        Left e ->
            case words s of
                [] -> Just PassCommand
                ["quit"] -> Just QuitCommand
                ["help"] -> Just HelpCommand
                ["show",     name] -> Just (ShowCommand ShowDefault name)
                ["show", form, name] -> case form of
                   "cnf"   -> Just (ShowCommand ShowCNF   name)
                   "dnf"   -> Just (ShowCommand ShowDNF   name)
                   "anf"   -> Just (ShowCommand ShowANF   name)
                   "table" -> Just (ShowCommand ShowTable name)
                   _ -> Nothing
                ["class",   name]  -> Just (ClassCommand name)
                ("complete":names) -> Just (CompleteCommand names)
                ["clean"] -> Just CleanCommand
                ["reset"] -> Just ResetCommand
                ["operators"] -> Just OperatorsCommand
                _ -> Nothing
