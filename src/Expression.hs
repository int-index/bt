{-# LANGUAGE GADTs, LambdaCase #-}

module Expression where

import Control.Applicative hiding (Const)
import Control.Lens hiding (Const)
import Control.Monad
import Data.List
import Data.Bool
import Data.Function

data Expression where
    Const  :: Bool -> Expression
    Access :: String -> Expression
    Not  :: Expression -> Expression
    And  :: Expression -> Expression -> Expression
    Or   :: Expression -> Expression -> Expression
    Xor  :: Expression -> Expression -> Expression
    Nand :: Expression -> Expression -> Expression
    Nor  :: Expression -> Expression -> Expression
    Ent  :: Expression -> Expression -> Expression
    Equ  :: Expression -> Expression -> Expression

data Function where
    Function :: [String] -> Expression -> Function
    Table :: [Bool] -> Function

instance Eq Function where
    (==) = (==) `on` tableOf

eval :: Function -> [Bool] -> Bool
eval (Function params e) args = value e where
    value :: Expression -> Bool
    value (Const a) = a
    value (Access name)
        = maybe (error $ "eval: Bad access " ++ name) id
        $ lookup name (zip params args)
    value (Not  x) = not (value x)
    value (And  x y) = value x && value y
    value (Or   x y) = value x || value y
    value (Xor  x y) = value x /= value y
    value (Nand x y) = not $ value x && value y
    value (Nor  x y) = not $ value x || value y
    value (Ent  x y) = not (value x) || value y
    value (Equ  x y) = value x == value y
eval (Table t) args = maybe (error "eval: Bad table") id $ t ^? ix (indexOf args)
    where indexOf = sum . zipWith (*) (powersOf 2) . reverse . map (bool 0 1)
          powersOf n = iterate (*n) 1

function :: Expression -> Function
function e = Function (names e) e

names :: Expression -> [String]
names e = (sort . nub) (names' e)

names' :: Expression -> [String]
names' (Const _) = []
names' (Access name) = [name]
names' (Not  x) = names' x
names' (And  x y) = names' x ++ names' y
names' (Or   x y) = names' x ++ names' y
names' (Xor  x y) = names' x ++ names' y
names' (Nand x y) = names' x ++ names' y
names' (Nor  x y) = names' x ++ names' y
names' (Ent  x y) = names' x ++ names' y
names' (Equ  x y) = names' x ++ names' y

tablify :: Function -> Function
tablify fun = Table (tableOf fun)

tableOf :: Function -> [Bool]
tableOf fun = map (eval fun) (argsOf fun)

nameStream :: [String]
nameStream = [1..] >>= flip replicateM alphabet
    where alphabet = ['a'..'z']

listOr [] = Const False
listOr xs = foldl1 Or xs

listAnd [] = Const True
listAnd xs = foldl1 And xs

data Form = Conjunctive | Disjunctive

nf :: Form -> Function -> Function
nf form fun = Function params (cat expr) where
    params = paramsOf fun
    expr = map term $ filter (not1 . eval fun) (argsOf fun)
    term = zipWith prim (map Access params)
    prim p = bool p (Not p) . not2
    cat = concat1 . map concat2
    (concat1, concat2, not1, not2) = case form of
        Conjunctive -> (listAnd, listOr, not, id)
        Disjunctive -> (listOr, listAnd, id, not)

conjunctive = ($ Conjunctive)
disjunctive = ($ Disjunctive)

anf :: Function -> Function
anf fun = Function params expr where
    params = paramsOf fun
    expr = case terms of
        [] -> Const False
        ct -> foldl1 Xor ct
    terms = keep $ zip (map term $ argsOf fun) (map head $ columns $ tableOf fun)
    term args = listAnd (map Access $ keep $ zip params args)
    keep = map fst . filter snd

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
arity (Table t) = truncate $ logBase 2 (fromIntegral (length t))

data PostClass = T0 | T1 | S | M | L deriving (Read, Show, Eq)

check :: PostClass -> Function -> Bool
check T0 fun = (eval fun) (replicate (arity fun) False) == False
check T1 fun = (eval fun) (replicate (arity fun) True)  == True
check S  fun = and (map check1 (argsOf fun))
    where check1 args = not (eval fun args) == eval fun (map not args)
check M  fun = check1 (tableOf fun)
    where check1 [_] = True
          check1 xs = let (as, bs) = split xs in as <= bs && check1 as && check1 bs
          split xs = splitAt (length xs `div` 2) xs
check L  fun = let Function _ expr = anf fun in check1 expr
    where check1 (Xor x y) = check1 x && check1 y
          check1 (Const  _) = True
          check1 (Access _) = True
          check1 _ = False

postClasses :: Function -> [PostClass]
postClasses fun = filter (\c -> check c fun) [T0, T1, S, M, L]

complete :: [Function] -> Bool
complete [] = False
complete xs = null $ foldr1 intersect $ map postClasses xs
