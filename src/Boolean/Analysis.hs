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

model_true  = Tree $ T.unsafeFromList [True]
model_false = Tree $ T.unsafeFromList [False]
model_not   = Tree $ T.unsafeFromList [True , False]
model_and   = Tree $ T.unsafeFromList [False, False, False, True ]
model_or    = Tree $ T.unsafeFromList [False, True , True , True ]
model_xor   = Tree $ T.unsafeFromList [False, True , True , False]

foldl0_call f a = foldl0 (call_2 f) (call_0 a)

listOr  = foldl0_call <$> behavesLike model_or  <*> behavesLike model_false
listXor = foldl0_call <$> behavesLike model_xor <*> behavesLike model_false
listAnd = foldl0_call <$> behavesLike model_and <*> behavesLike model_true

instance NF Conjunctive where
    data RepNF Conjunctive = CNF [String] [[Either String String]]
    normalize fun = do
        let params = paramsOf fun
            args   = argsOf   fun
            term = zipWith prim params
            prim p = bool (Right p) (Left p) . not
        expr <- map term <$> filterM (evaluate fun) args
        return (CNF params expr)
    reifyNF (CNF params expr) = do
        call_not     <- call_1 <$> behavesLike model_not
        call_listOr  <- listOr
        call_listAnd <- listAnd
        let pass0 = either (call_not . Access) Access
        return $ Function params (nmap3 call_listOr call_listAnd pass0 expr)

instance NF Disjunctive where
    data RepNF Disjunctive = DNF [String] [[Either String String]]
    normalize fun = do
        let params = paramsOf fun
            args   = argsOf   fun
            term = zipWith prim params
            prim p = bool (Right p) (Left p)
        expr <- map term <$> filterM (fmap not . evaluate fun) args
        return (DNF params expr)
    reifyNF (DNF params expr) = do
        call_not     <- call_1 <$> behavesLike model_not
        call_listAnd <- listAnd
        call_listOr  <- listOr
        let pass0 = either (call_not . Access) Access
        return $ Function params (nmap3 call_listAnd call_listOr pass0 expr)

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
        call_listXor <- listXor
        call_listAnd <- listAnd
        return $ Function params (nmap3 call_listXor call_listAnd Access expr)

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
