{-# LANGUAGE GADTs, LambdaCase #-}

module Expression where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.List
import Data.Bool
import Data.Function
import qualified Data.Map as M

data Expression where
    Access :: String -> Expression
    Call :: String -> [Expression] -> Expression

data Function where
    Function :: [String] -> Expression -> Function
    Table :: [Bool] -> Function

funeq :: Definitions -> Function -> Function -> Bool
funeq defs = (==) `on` (tableOf defs)

call_0 :: String -> Expression
call_0 s     = Call s []

call_1 :: String -> Expression -> Expression
call_1 s x   = Call s [x]

call_2 :: String -> Expression -> Expression -> Expression
call_2 s x y = Call s [x, y]

type Definitions = M.Map String Function

predef :: Definitions
predef = M.fromList
    [ ("1"   , Table [True ])
    , ("0"   , Table [False])
    , ("id"  , Table [False, True])
    , ("not" , Table [True, False])
    , ("and" , Table [False, False, False, True ])
    , ("or"  , Table [False, True , True , True ])
    , ("xor" , Table [False, True , True , False])
    , ("nand", Table [True , True , True , False])
    , ("nor" , Table [True , False, False, False])
    , ("ent" , Table [True , True , False, True ])
    , ("equ" , Table [True , False, False, True ])
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
eval _ (Table t) args = maybe (error "eval: Bad table") id $ t ^? ix (indexOf args)
    where indexOf = sum . zipWith (*) (powersOf 2) . reverse . map (bool 0 1)
          powersOf n = iterate (*n) 1

function :: Expression -> Function
function e = Function (names e) e

names :: Expression -> [String]
names e = (sort . nub) (names' e)

names' :: Expression -> [String]
names' (Access name) = [name]
names' (Call _ xs) = concatMap names' xs

tablify :: Definitions -> Function -> Function
tablify defs fun = Table (tableOf defs fun)

tableOf :: Definitions -> Function -> [Bool]
tableOf defs fun = map (eval defs fun) (argsOf fun)

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
arity (Table t) = truncate $ logBase 2 (fromIntegral (length t))

data PostClass = T0 | T1 | S | M | L deriving (Read, Show, Eq)

check :: PostClass -> Definitions -> Function -> Bool
check T0 defs fun = (eval defs fun) (replicate (arity fun) False) == False
check T1 defs fun = (eval defs fun) (replicate (arity fun) True)  == True
check S  defs fun = and (map check1 (argsOf fun))
    where check1 args = not (eval defs fun args) == eval defs fun (map not args)
check M  defs fun = check1 (tableOf defs fun)
    where check1 [_] = True
          check1 xs = let (as, bs) = split xs in as <= bs && check1 as && check1 bs
          split xs = splitAt (length xs `div` 2) xs
check L  defs fun = (check1 . snd) (anf' defs fun)
    where check1 = and . map (liftA2 (||) null single)
          single [_] = True
          single  _  = False

postClasses :: Definitions -> Function -> [PostClass]
postClasses defs fun = filter (\c -> check c defs fun) [T0, T1, S, M, L]

complete :: Definitions -> [Function] -> Bool
complete _ [] = False
complete defs xs = null $ foldr1 intersect $ map (postClasses defs) xs
