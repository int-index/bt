{-# LANGUAGE MultiWayIf, LambdaCase #-}
module Parser where

import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.State

import qualified Expression as E

data Token = One    | Zero
           | LParen | RParen
           | LSqbr  | RSqbr
           | Not  | And | Or  | Xor
           | Nand | Nor | Ent | Equ
           | Name String
           | Bad Char
           deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs) = case c of
    '1' -> One  : rt
    '0' -> Zero : rt
    '(' -> LParen : rt
    ')' -> RParen : rt
    '[' -> LSqbr  : rt
    ']' -> RSqbr  : rt
    '-' -> Not  : rt
    '&' -> And  : rt
    '|' -> Or   : rt
    '+' -> Xor  : rt
    '↑' -> Nand : rt
    '↓' -> Nor  : rt
    '→' -> Ent  : rt
    '~' -> Equ  : rt
    _ -> if | isAlpha c -> tokenizeName c cs
            | isSpace c -> rt
            | otherwise -> Bad c : rt
    where rt = tokenize cs

tokenizeName :: Char -> String -> [Token]
tokenizeName c cs = mangle (c:x) : tokenize xs
    where (x, xs) = span isAlphaNum cs
          mangle "not"  = Not
          mangle "and"  = And
          mangle "or"   = Or
          mangle "xor"  = Xor
          mangle "nand" = Nand
          mangle "nor"  = Nor
          mangle "ent"  = Ent
          mangle "equ"  = Equ
          mangle cs = Name cs

parse :: String -> Maybe E.Function
parse cs = evalStateT parser (tokenize cs)

fallback a = (<|> pure a)

sepr elemTr opTr = tr where
    tr = elemTr >>= next
    next a = fallback a $ opTr <*> return a <*> tr

sepl elemTr opTr = elemTr >>= next where
    next a = fallback a $ (opTr <*> return a <*> elemTr) >>= next


type Parser a = StateT [Token] Maybe a

parser :: Parser E.Function
parser = (E.function <$> pEqu <|> pTable) <* pEnd

pEqu :: Parser E.Expression
pEqu = sepl pEnt (E.call_2 "equ" <$ pExpect Equ)

pEnt :: Parser E.Expression
pEnt = sepr pOr (E.call_2 "ent" <$ pExpect Ent)

pOr :: Parser E.Expression
pOr = sepl pAnd (E.call_2 "or" <$ pExpect Or)

pAnd :: Parser E.Expression
pAnd = sepl pOther (E.call_2 "and" <$ pExpect And)

pOther :: Parser E.Expression
pOther = sepl (pNot <*> pPrim) $ pHead >>= \case
    Xor  -> return (E.call_2 "xor")
    Nand -> return (E.call_2 "nand")
    Nor  -> return (E.call_2 "nor")
    _ -> mzero

pNot :: Parser (E.Expression -> E.Expression)
pNot = maybe id (const $ E.call_1 "not") <$> optional (pExpect Not)

pPrim :: Parser E.Expression
pPrim = pHead >>= \case
    One  -> return (E.call_0 "1")
    Zero -> return (E.call_0 "0")
    Name s -> return (E.Access s)
    LParen -> pEqu <* pExpect RParen
    _ -> mzero

pTable :: Parser E.Function
pTable = pExpect LSqbr *> (E.Table <$> many pX) <* pExpect RSqbr where
    pX = pHead >>= \case
        One  -> return True
        Zero -> return False
        _ -> mzero

pExpect :: Token -> Parser Token
pExpect a = mfilter (==a) pHead

pHead :: Parser Token
pHead = StateT $ \case
    (c:cs) -> return (c, cs)
    _ -> mzero

pEnd :: Parser ()
pEnd = StateT $ \case
    [] -> return ((), [])
    _ -> mzero
