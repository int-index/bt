{-# LANGUAGE LambdaCase #-}
module Analysis where

import Control.Applicative
import Data.List
import Data.Bool
import Data.Function

import Expression

import qualified Tree as T

foldl0 f b = \case
    [] -> b
    xs -> foldl1 f xs

--
-- Conjunctive, disjunctive and algebraic normal forms
-- 

data Form = Conjunctive | Disjunctive

nf :: Form -> Definitions -> Function -> Function
nf form defs fun = Function params (pass2 . map pass1 . map (map pass0) $ expr) where
    params = paramsOf fun
    args   = argsOf   fun
    expr = map term $ filter (not1 . eval defs fun) args
    term = zipWith prim params
    prim p = bool (Right p) (Left p) . not2

    pass0 = either (call_1 "not" . Access) Access

    (pass2, pass1, not1, not2) = case form of
        Conjunctive -> (listAnd, listOr, not, id)
        Disjunctive -> (listOr, listAnd, id, not)

    listOr  = foldl0 (call_2 "or")  (call_0 "false")
    listAnd = foldl0 (call_2 "and") (call_0 "true")


conjunctive = ($ Conjunctive)
disjunctive = ($ Disjunctive)

anf :: Definitions -> Function -> Function
anf defs fun = Function params (pass2 . map pass1 . map (map pass0) $ expr)
    where (params, expr) = anf' defs fun
          pass0 = Access
          pass1 = foldl0 (call_2 "and") (call_0 "true")
          pass2 = foldl0 (call_2 "xor") (call_0 "false")

anf' :: Definitions -> Function -> ([String], [[String]])
anf' defs fun = (params, expr) where
    params = paramsOf fun
    args   = argsOf   fun
    table  = tableOf defs fun
    expr = keep (map (keep params) args) (map head $ columns table)
    keep xs ys = map fst . filter snd $ zip xs ys


---
--- Post's classes and functional completeness
---

data PostClass = T0 | T1 | S | M | L deriving (Read, Show, Eq)

check :: PostClass -> Definitions -> Function -> Bool
check T0 defs fun = (eval defs fun) (replicate (arity fun) False) == False
check T1 defs fun = (eval defs fun) (replicate (arity fun) True)  == True
check S  defs fun = and (map check1 (argsOf fun))
    where check1 args = not (eval defs fun args) == eval defs fun (map not args)
check M  defs fun = check1 (treeOf defs fun)
    where check1 (T.Node _) = True
          check1 (T.Joint f t) = check1 f && check1 t && on (<=) T.toList f t
check L  defs fun = (check1 . snd) (anf' defs fun)
    where check1 = and . map (liftA2 (||) null single)
          single [_] = True
          single  _  = False

postClasses :: Definitions -> Function -> [PostClass]
postClasses defs fun = filter (\c -> check c defs fun) [T0, T1, S, M, L]

complete :: Definitions -> [Function] -> Bool
complete _ [] = False
complete defs xs = null $ foldr1 intersect $ map (postClasses defs) xs
