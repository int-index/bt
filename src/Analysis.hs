{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Analysis where

import Control.Applicative
import Data.List
import Data.Bool
import Data.Function
import Data.Proxy

import Expression

import qualified Tree as T

foldl0 f b = \case
    [] -> b
    xs -> foldl1 f xs

-- nested map
nmap3 f g h = f . fmap (g . fmap h)


--
-- Conjunctive, disjunctive and algebraic normal forms
-- 

data Conjunctive
data Disjunctive
data Algebraic

class NF a where
    data family RepNF a :: *
    reifyNF :: RepNF a -> Function
    normalize :: Definitions -> Function -> RepNF a

nf :: forall form . NF form => Proxy form -> Definitions -> Function -> Function
nf Proxy defs fun = reifyNF (normalize defs fun :: RepNF form)

conjunctive f = f (Proxy :: Proxy Conjunctive)
disjunctive f = f (Proxy :: Proxy Disjunctive)
algebraic   f = f (Proxy :: Proxy Algebraic)


listOr  = foldl0 (call_2 "or")  (call_0 "false")
listXor = foldl0 (call_2 "xor") (call_0 "false")
listAnd = foldl0 (call_2 "and") (call_0 "true")


instance NF Conjunctive where
    data RepNF Conjunctive = CNF [String] [[Either String String]]
    normalize defs fun = CNF params expr where
        params = paramsOf fun
        args   = argsOf   fun
        expr = map term $ filter (eval defs fun) args
        term = zipWith prim params
        prim p = bool (Right p) (Left p) . not
    reifyNF (CNF params expr) = Function params (nmap3 listOr listAnd pass0 expr) where
        pass0 = either (call_1 "not" . Access) Access

instance NF Disjunctive where
    data RepNF Disjunctive = DNF [String] [[Either String String]]
    normalize defs fun = DNF params expr where
        params = paramsOf fun
        args   = argsOf   fun
        expr = map term $ filter (not . eval defs fun) args
        term = zipWith prim params
        prim p = bool (Right p) (Left p)
    reifyNF (DNF params expr) = Function params (nmap3 listAnd listOr pass0 expr) where
        pass0 = either (call_1 "not" . Access) Access

instance NF Algebraic where
    data RepNF Algebraic = ANF [String] [[String]]
    normalize defs fun = ANF params expr where
        params = paramsOf fun
        args   = argsOf   fun
        table  = tableOf defs fun
        expr = keep (map (keep params) args) (map head $ columns table)
        keep xs ys = map fst . filter snd $ zip xs ys
    reifyNF (ANF params expr) = Function params (nmap3 listXor listAnd Access expr)

anf_core :: RepNF Algebraic -> [[String]]
anf_core (ANF _ core) = core

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
check L  defs fun = check1 (anf_core $ normalize defs fun)
    where check1 = and . map (liftA2 (||) null single)
          single [_] = True
          single  _  = False

postClasses :: Definitions -> Function -> [PostClass]
postClasses defs fun = filter (\c -> check c defs fun) [T0, T1, S, M, L]

complete :: Definitions -> [Function] -> Bool
complete _    [] = False
complete defs xs = null $ foldr1 intersect $ map (postClasses defs) xs
