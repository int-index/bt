{-# LANGUAGE LambdaCase, TupleSections #-}
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
    | null params = discarding rExpression e
    | otherwise   = unwords params ++ " . " ++ discarding rExpression e
rFunction (Tree t) = "[" ++ map (bool '0' '1') (T.toList t) ++ "]"

rExpression :: Expression -> Leveling String
rExpression (Access name)  = solid name
rExpression (Call name xs) = case map rExpression xs of
    [   ] -> rNullary name
    [x  ] -> rUnary   name x
    [x,y] -> rBinary  name x y
    args  -> rCall    name args


aliasLookup :: String -> Maybe String
aliasLookup name = M.lookup name operators >>= \op -> listToMaybe (aliases op)

rNullary :: String -> Leveling String
rNullary name = maybe (rCall name []) solid
              $ aliasLookup name

-- TODO
rUnary :: String -> Leveling String -> Leveling String
rUnary name x = rCall name [x]

-- TODO
rBinary :: String -> Leveling String -> Leveling String -> Leveling String
rBinary name x y = rCall name [x, y]

rCall :: String -> [Leveling String] -> Leveling String
rCall name xs = solid $ name ++ parens (intercalate ", " $ discarding xs)

parens :: String -> String
parens s = "(" ++ s ++ ")"

solid = (,Solid)

discarding :: Functor f => f (a, b) -> f a
discarding = fmap fst

data Level = Solid
           | L1 Int Fixity1
           | L2 Int Fixity2

type Leveling t = (t, Level)
