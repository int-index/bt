module Boolean.Expression where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
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

call_0 :: String -> Expression
call_0 s     = Call s []

call_1 :: String -> Expression -> Expression
call_1 s x   = Call s [x]

call_2 :: String -> Expression -> Expression -> Expression
call_2 s x y = Call s [x, y]

type Definitions = M.Map String Function

data Error = BadAccess String
           | BadCall   String
           | BadModel
           | BadTree
    deriving (Eq, Show)

type Evaluate a = ReaderT Definitions (Except Error) a

runEvaluate :: Definitions -> (Error -> x) -> (a -> x) -> Evaluate a -> x
runEvaluate defs handle ret m = either handle ret (runExcept (runReaderT m defs))

evaluate :: Function -> [Bool] -> Evaluate Bool
evaluate (Function params e) args = value e where
    value (Access name)
        = maybe (throwError $ BadAccess name) return
        $ lookup name (zip params args)
    value (Call name xs) = withFunction name
        $ \fun -> evaluate fun =<< mapM value xs
evaluate (Tree t)  args = maybe (throwError BadTree) return (T.visit t args)

onFunction :: (Function -> Evaluate a) -> (String -> Evaluate a)
onFunction f name = maybe (throwError $ BadCall name) f =<< asks (M.lookup name)

withFunction = flip onFunction

liftL2 :: ((a      -> c) -> (b      -> c))
       -> ((a -> a -> c) -> (b -> b -> c))
liftL2 k f a1 a2 =
    flip k a1 $ \b1 ->
    flip k a2 $ \b2 ->
      f b1 b2

funeq :: Function -> Function -> Evaluate Bool
funeq = liftA2 (==) `on` tableOf

function :: Expression -> Function
function e = Function (names e) e

names :: Expression -> [String]
names e = (sort . nub) (names' e)
  where names' (Access name) = [name]
        names' (Call _ xs) = concatMap names' xs

tablify :: Function -> Evaluate Function
tablify fun = Tree <$> treeOf fun

tableOf :: Function -> Evaluate [Bool]
tableOf fun = mapM (evaluate fun) (argsOf fun)

treeOf :: Function -> Evaluate (T.Tree Bool)
treeOf (Tree t) = return t
treeOf fun = T.unsafeFromList <$> tableOf fun

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
