module Boolean.Render where

import Data.Bool
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Foldable

import Boolean.Operator
import Boolean.Expression

import qualified Data.Boolean.Tree as T
import qualified Data.Map as M

rFunction :: Operators -> Function -> String
rFunction ops (Function params e)
    | null params = discarding rExpr e
    | otherwise   = unwords params ++ " . " ++ discarding rExpr e
    where rExpr = rExpression ops
rFunction _ (Tree t) = "[" ++ map (bool '0' '1') (T.toList t) ++ "]"

rExpression :: Operators -> Expression -> Leveling String
rExpression _   (Access name)  = (name, L0)
rExpression ops (Call name xs) = maybe (rCall name args) id mresult
    where args = rExpression ops `map` xs
          mresult  = do
              op <- M.lookup name ops
              getFirst $ foldMap (\r -> First (r op args)) handlers
          handlers = [rNullary, rUnary, rBinary]

type Handler = Operator -> [Leveling String] -> Maybe (Leveling String)

rNullary :: Handler
rNullary (NullaryOperator aliases) [] = do
    alias <- listToMaybe aliases
    return (alias, L0)
rNullary _ _ = Nothing

rUnary :: Handler
rUnary (UnaryOperator aliases fxt) [x] = do
    alias <- listToMaybe aliases
    return (wrapHard fixity x `conc` alias, fixity)
    where fixity = uncurry L1 fxt
          conc = case snd fxt of
              Prefix  -> flip (++)
              Postfix ->      (++)
rUnary _ _ = Nothing

rBinary :: Handler
rBinary (BinaryOperator aliases fxt) [x, y] = do
    alias <- listToMaybe aliases
    return (lhs ++ " " ++ alias ++ " " ++ rhs, fixity)
    where fixity = uncurry L2 fxt
          (wrap1, wrap2) = case snd fxt of
              Nonfix   -> (wrapHard, wrapHard)
              Leftfix  -> (wrapSoft, wrapHard)
              Rightfix -> (wrapHard, wrapSoft)
          lhs = wrap1 fixity x
          rhs = wrap2 fixity y
rBinary _ _ = Nothing

wrapHard, wrapSoft :: Level -> (String, Level) -> String
wrapHard p (a, q) = bool parens id (p `weaker` q) a
wrapSoft p (a, q) = bool parens id (p `weaker` q || p == q) a

rCall :: String -> [Leveling String] -> Leveling String
rCall name xs = (name ++ parens (intercalate ", " $ discarding xs), L0)

parens :: String -> String
parens s = "(" ++ s ++ ")"

discarding :: Functor f => f (a, b) -> f a
discarding = fmap fst

weaker :: Level -> Level -> Bool
weaker lx ly = case (value lx, value ly) of
    (Nothing, _) -> False
    (_, Nothing) -> True
    (Just i, Just j) -> i > j
    where value L0 = Nothing
          value (L1 i _) = Just i
          value (L2 i _) = Just i

data Level = L0
           | L1 Int Fixity1
           | L2 Int Fixity2
           deriving (Eq)

type Leveling t = (t, Level)
