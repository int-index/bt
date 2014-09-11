{-# LANGUAGE LambdaCase #-}

module Expression where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Bool
import Data.Function
import qualified Data.Map as M

import qualified Tree as T

data Expression
    = Access String
    | Call   String [Expression]

data Function
    = Function [String] Expression
    | Tree     T.Tree

funeq :: Definitions -> Function -> Function -> Bool
funeq defs = (==) `on` (tableOf defs)

call_0 :: String -> Expression
call_0 s     = Call s []

call_1 :: String -> Expression -> Expression
call_1 s x   = Call s [x]

call_2 :: String -> Expression -> Expression -> Expression
call_2 s x y = Call s [x, y]

type Definitions = M.Map String Function

tree :: [Bool] -> Function
tree = Tree . T.unsafeFromList

predef :: Definitions
predef = M.fromList
    [ ("1"   , tree [True])
    , ("0"   , tree [False])
    , ("id"  , tree [False, True])
    , ("not" , tree [True, False])
    , ("and" , tree [False, False, False, True ])
    , ("or"  , tree [False, True , True , True ])
    , ("xor" , tree [False, True , True , False])
    , ("nand", tree [True , True , True , False])
    , ("nor" , tree [True , False, False, False])
    , ("ent" , tree [True , True , False, True ])
    , ("equ" , tree [True , False, False, True ])
    ]

eval :: Definitions -> Function -> [Bool] -> Bool
eval defs (Function params e) args = value e where
    value :: Expression -> Bool
    value (Access name)
        = maybe (error $ "eval: Bad access " ++ name) id
        $ lookup name (zip params args)
    value (Call name xs) = case M.lookup name defs of
        Nothing  -> error $ "eval: Bad call " ++ name
        Just fun -> eval defs fun (map value xs)
eval _ (Tree t)  args = maybe (error "eval: Bad tree") id (T.visit t args)

function :: Expression -> Function
function e = Function (names e) e

names :: Expression -> [String]
names e = (sort . nub) (names' e)

names' :: Expression -> [String]
names' (Access name) = [name]
names' (Call _ xs) = concatMap names' xs

tablify :: Definitions -> Function -> Function
tablify defs fun = tree (tableOf defs fun)

tableOf :: Definitions -> Function -> [Bool]
tableOf defs fun = map (eval defs fun) (argsOf fun)

treeOf :: Definitions -> Function -> T.Tree
treeOf _ (Tree t) = t
treeOf defs fun   = T.unsafeFromList (tableOf defs fun)

nameStream :: [String]
nameStream = [1..] >>= flip replicateM alphabet
    where alphabet = ['a'..'z']


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

    listOr  = foldl0 (call_2 "or")  (call_0 "0")
    listAnd = foldl0 (call_2 "and") (call_0 "1")


conjunctive = ($ Conjunctive)
disjunctive = ($ Disjunctive)


foldl0 f b = \case
    [] -> b
    xs -> foldl1 f xs

anf :: Definitions -> Function -> Function
anf defs fun = Function params (pass2 . map pass1 . map (map pass0) $ expr)
    where (params, expr) = anf' defs fun
          pass0 = Access
          pass1 = foldl0 (call_2 "and") (call_0 "1")
          pass2 = foldl0 (call_2 "xor") (call_0 "0")

anf' :: Definitions -> Function -> ([String], [[String]])
anf' defs fun = (params, expr) where
    params = paramsOf fun
    args   = argsOf   fun
    table  = tableOf defs fun
    expr = keep (map (keep params) args) (map head $ columns table)
    keep xs ys = map fst . filter snd $ zip xs ys

columns :: [Bool] -> [[Bool]]
columns [] = []
columns xs = let ys = zipWith (/=) xs (tail xs) in xs : columns ys

argsOf :: Function -> [[Bool]]
argsOf = variants . arity

paramsOf :: Function -> [String]
paramsOf (Function params _) = params
paramsOf fun = take (arity fun) nameStream

variants :: Int -> [[Bool]]
variants 0 = [[]]
variants n = (:) <$> [False, True] <*> variants (n - 1)

arity :: Function -> Int
arity (Function params _) = length params
arity (Tree t) = T.depth t

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
