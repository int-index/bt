{-# LANGUAGE LambdaCase #-}
module Render where

import Data.Bool
import Data.List
import Data.Maybe

import Operator
import Expression
import qualified Tree as T

import qualified Data.Map as M

{-
parens :: String -> String
parens = ("("++) . (++")")

data Level
    = ALevel Integer Fixity
    | HLevel
    | SLevel

whatfix op = maybe (9, LFix) id (lookup op fixtable)
    where fixtable =
            [ ("&", (7, LFix))
            , ("|", (6, LFix))
            , ("→", (5, RFix))
            , ("~", (4, LFix))
            ]

renderBinary :: String -> Expression -> Expression -> (String, Level)
renderBinary op expr1 expr2
    = (lhs ++ " " ++ op ++ " " ++ rhs, uncurry ALevel opfix)
    where
        opfix = whatfix op
        (operand1, level1) = renderExpr expr1
        (operand2, level2) = renderExpr expr2
        defaultHandler n m _
            | m > n = id
            | otherwise = parens
        advHandler n f m g
            | m > n = id
            | f == g && m == n = id
            | otherwise = parens
        (handler1, handler2) = case opfix of
            (n, NFix) -> (\a -> (a, a)) (defaultHandler n)
            (n, LFix) -> (advHandler n LFix, defaultHandler n)
            (n, RFix) -> (defaultHandler n, advHandler n RFix)
        wrap handler = \case
            SLevel -> parens
            HLevel -> id
            ALevel n fix -> handler n fix
        lhs = wrap handler1 level1 operand1
        rhs = wrap handler2 level2 operand2

hlevel = \case
    HLevel -> id
    _ -> parens

renderExpr :: Expression -> (String, Level)
renderExpr = \case
    Access name -> (name, HLevel)
    Call "1"   []  -> ("1", HLevel)
    Call "0"   []  -> ("0", HLevel)
    Call "not" [x] -> let (s, level) = renderExpr x
                      in ("-" ++ hlevel level s, HLevel)
    Call "and" [x, y] -> renderBinary "&" x y
    Call "or"  [x, y] -> renderBinary "|" x y
    Call "xor" [x, y] -> renderBinary "+" x y
    Call "nand"[x, y] -> renderBinary "↑" x y
    Call "nor" [x, y] -> renderBinary "↓" x y
    Call "ent" [x, y] -> renderBinary "→" x y
    Call "equ" [x, y] -> renderBinary "~" x y
    Call name xs -> (name ++ rarx, HLevel) where
        rarx | null xs   = ""
             | otherwise = parens
                         . intercalate ", "
                         . map (fst . renderExpr)
                         $ xs

instance Show Expression where
    show = fst . renderExpr
-}

rFunction :: Function -> String
rFunction (Function params e)
    | null params = rExpression e
    | otherwise   = unwords params ++ " . " ++ rExpression e
rFunction (Tree t) = "[" ++ map (bool '0' '1') (T.toList t) ++ "]"

rAlias :: String -> String
rAlias name = maybe name id
            $ do op <- M.lookup name operators
                 listToMaybe (aliases op)

rExpression :: Expression -> String
rExpression (Access name)  = name
rExpression (Call name []) = rAlias name
rExpression (Call name xs) = name ++ "(" ++ intercalate ", " (map rExpression xs) ++ ")"
