{-# LANGUAGE FlexibleContexts #-}
module Boolean.Parser where

import Control.Applicative
import Data.List
import Data.Ord
import qualified Data.Map as M

import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Parser.Combinators
import Text.Parser.Expression
import qualified Text.Parsec as Parsec

import qualified Boolean.Expression as E
import qualified Boolean.Operator   as O
import qualified Data.Boolean.Tree  as T

pFunction :: O.Operators -> Parsec.Parsec String () E.Function
pFunction ops = pTable <|> E.function <$> pExpression ops

pTable = do
    bs <- brackets (Parsec.many bin)
    case T.fromList bs of
        Nothing -> Parsec.parserFail "broken table"
        Just t  -> return (E.Tree t)
  where
    one  = Parsec.char '1'
    zero = Parsec.char '0'
    bin  = True <$ one <|> False <$ zero

pExpression ops = pExpr where

    pExpr = buildExpressionParser table pTerm

    pTerm = parens pExpr <|> pPrim <|> pNullary

    pPrim = do
        name <- ident emptyIdents
        pCall name <|> return (E.Access name)

    pCall name = do
        params <- parens (pExpr `sepBy` comma)
        return $ E.Call name params

    pNullary
        = token
        $ choice . map mkParser
        $ sortBy (flip . comparing $ length . snd)
        $ M.toList ops >>= gather
      where
        gather (name, O.NullaryOperator aliases) = (name,) <$> aliases
        gather _ = []
        mkParser (r, s) = try (E.call_0 r <$ Parsec.string s)


    table = groupByFst (M.toList ops >>= gather)
      where
        gather (name, O.UnaryOperator aliases (i, fixity)) = do
            alias <- aliases
            let desc = wx (E.call_1 name <$ op alias)
                wx = case fixity of
                  O.Prefix  -> Prefix
                  O.Postfix -> Postfix
            return (i, desc)
        gather (name, O.BinaryOperator aliases (i, fixity)) = do
            alias <- aliases
            let desc = Infix (E.call_2 name <$ op alias) wx
                wx = case fixity of
                  O.Leftfix  -> AssocLeft
                  O.Rightfix -> AssocRight
                  O.Nonfix   -> AssocNone
            return (i, desc)
        gather _ = []

op :: String -> Parsec.Parsec String () String
op = try . token . Parsec.string

groupByFst :: Ord k => [(k, a)] -> [[a]]
groupByFst = M.elems . M.fromListWith (++) . map (\(k,a) -> (k,[a]))
