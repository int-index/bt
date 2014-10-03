{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Boolean.Analysis where

import Control.Monad.Reader
import Control.Applicative
import Data.List
import Data.Bool
import Data.Function
import Data.Proxy

import Boolean.Expression
import qualified Data.Boolean.Tree as T

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
    normalize :: Function -> Evaluate (RepNF a)

nf :: forall form . NF form => Proxy form -> Function -> Evaluate Function
nf Proxy fun = reifyNF <$> (normalize fun :: Evaluate (RepNF form))

conjunctive f = f (Proxy :: Proxy Conjunctive)
disjunctive f = f (Proxy :: Proxy Disjunctive)
algebraic   f = f (Proxy :: Proxy Algebraic)


listOr  = foldl0 (call_2 "or")  (call_0 "false")
listXor = foldl0 (call_2 "xor") (call_0 "false")
listAnd = foldl0 (call_2 "and") (call_0 "true")


instance NF Conjunctive where
    data RepNF Conjunctive = CNF [String] [[Either String String]]
    normalize fun = do
        let params = paramsOf fun
            args   = argsOf   fun
            term = zipWith prim params
            prim p = bool (Right p) (Left p) . not
        expr <- map term <$> filterM (evaluate fun) args
        return (CNF params expr)
    reifyNF (CNF params expr) = Function params (nmap3 listOr listAnd pass0 expr) where
        pass0 = either (call_1 "not" . Access) Access

instance NF Disjunctive where
    data RepNF Disjunctive = DNF [String] [[Either String String]]
    normalize fun = do
        let params = paramsOf fun
            args   = argsOf   fun
            term = zipWith prim params
            prim p = bool (Right p) (Left p)
        expr <- map term <$> filterM (fmap not . evaluate fun) args
        return (DNF params expr)
    reifyNF (DNF params expr) = Function params (nmap3 listAnd listOr pass0 expr) where
        pass0 = either (call_1 "not" . Access) Access

instance NF Algebraic where
    data RepNF Algebraic = ANF [String] [[String]]
    normalize fun = do
        table <- tableOf fun
        let params = paramsOf fun
            args   = argsOf   fun
            expr = keep (map (keep params) args) (map head $ columns table)
            keep xs ys = map fst . filter snd $ zip xs ys
        return (ANF params expr)
    reifyNF (ANF params expr) = Function params (nmap3 listXor listAnd Access expr)

anf_core :: RepNF Algebraic -> [[String]]
anf_core (ANF _ core) = core

---
--- Post's classes and functional completeness
---

data PostClass = T Bool | S | M | L deriving (Eq)

instance Show PostClass where
    show = \case
        T False -> "T0"
        T True  -> "T1"
        S -> "S"
        M -> "M"
        L -> "L"

check :: Function -> PostClass -> Evaluate Bool
check fun (T a) = (==a) <$> evaluate fun (arity fun `replicate` a)
check fun S  = and <$> mapM check1 (argsOf fun)
    where check1 args = do
            normal  <- evaluate fun args
            negated <- evaluate fun (map not args)
            return (not normal == negated)
check fun M  = check1 <$> treeOf fun
    where check1 (T.Node _) = True
          check1 (T.Joint f t) = check1 f && check1 t && on (<=) T.toList f t
check fun L  = all less1 . anf_core <$> normalize fun
    where less1 [ ] = True
          less1 [_] = True
          less1  _  = False

postClasses :: Function -> Evaluate [PostClass]
postClasses fun = filterM (check fun) [T False, T True, S, M, L]

complete :: [Function] -> Evaluate Bool
complete []   = return False
complete funs = null . foldr1 intersect <$> mapM postClasses funs
