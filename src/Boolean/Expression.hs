module Boolean.Expression where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Function
import qualified Data.Map as M

import qualified Data.Boolean.Tree as T

data Expression
    = Access String
    | Call   String [Expression]

data Function
    = Function [String] Expression
    | Tree     (T.Tree Bool)

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
    [ ("true" , tree [True])
    , ("false", tree [False])
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

treeOf :: Definitions -> Function -> (T.Tree Bool)
treeOf _ (Tree t) = t
treeOf defs fun   = T.unsafeFromList (tableOf defs fun)

nameStream :: [String]
nameStream = [1..] >>= flip replicateM alphabet
    where alphabet = ['a'..'z']

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
