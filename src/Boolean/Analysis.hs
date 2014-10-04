{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Boolean.Analysis where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative
import Data.List
import Data.Bool
import Data.Function
import Data.Proxy
import qualified Data.Map as M

import Boolean.Expression
import Boolean.Reflection
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
    reifyNF :: RepNF a -> Evaluate Function
    normalize :: Function -> Evaluate (RepNF a)

nf :: forall form . NF form => Proxy form -> Function -> Evaluate Function
nf Proxy fun = (normalize fun :: Evaluate (RepNF form)) >>= reifyNF

conjunctive f = f (Proxy :: Proxy Conjunctive)
disjunctive f = f (Proxy :: Proxy Disjunctive)
algebraic   f = f (Proxy :: Proxy Algebraic)

behavesLike :: Function -> Evaluate String
behavesLike model
      = asks M.toList
    >>= filterM (funeq model . snd)
    >>= \case
          []    -> throwError BadModel
          def:_ -> return (fst def)

summon0 f = call_0 <$> behavesLike (reflect0 f)
summon1 f = call_1 <$> behavesLike (reflect1 f)
summon2 f = call_2 <$> behavesLike (reflect2 f)

listOr  = foldl0 <$> summon2 (||) <*> summon0 False
listXor = foldl0 <$> summon2 (/=) <*> summon0 False
listAnd = foldl0 <$> summon2 (&&) <*> summon0 True

instance NF Conjunctive where
    data RepNF Conjunctive = CNF [String] [[(Bool, String)]]
    normalize fun = do
        let params = paramsOf fun
            args   = argsOf   fun
            term = zipWith prim params
            prim = flip (,)
        expr <- map term <$> filterM (evaluate fun) args
        return (CNF params expr)
    reifyNF (CNF params expr) = do
        pass2 <- listOr
        pass1 <- listAnd
        pass0 <- do
            call_not <- summon1 not
            return $ uncurry (bool call_not id) . fmap Access
        return $ Function params (nmap3 pass2 pass1 pass0 expr)

instance NF Disjunctive where
    data RepNF Disjunctive = DNF [String] [[(Bool, String)]]
    normalize fun = do
        let params = paramsOf fun
            args   = argsOf   fun
            term = zipWith prim params
            prim = flip (,)
        expr <- map term <$> filterM (fmap not . evaluate fun) args
        return (DNF params expr)
    reifyNF (DNF params expr) = do
        pass2 <- listAnd
        pass1 <- listOr
        pass0 <- do
            call_not <- summon1 not
            return $ uncurry (bool id call_not) . fmap Access
        return $ Function params (nmap3 pass2 pass1 pass0 expr)

instance NF Algebraic where
    data RepNF Algebraic = ANF [String] [[String]]
    normalize fun = do
        table <- tableOf fun
        let params = paramsOf fun
            args   = argsOf   fun
            expr = keep (map (keep params) args) (map head $ columns table)
            keep xs ys = map fst . filter snd $ zip xs ys
        return (ANF params expr)
    reifyNF (ANF params expr) = do
        pass2 <- listXor
        pass1 <- listAnd
        pass0 <- return Access
        return $ Function params (nmap3 pass2 pass1 pass0 expr)

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
