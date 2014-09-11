module Tree where

import Data.Maybe
import Control.Applicative

data Tree = Joint Tree Tree | Node Bool
    deriving Show

toList :: Tree -> [Bool]
toList (Node b) = [b]
toList (Joint f t) = toList f ++ toList t

unsafeFromList :: [Bool] -> Tree
unsafeFromList = fromJust . fromList

fromList :: [Bool] -> Maybe Tree
fromList = joint . map Node where
    joint2 :: [Tree] -> Maybe [Tree]
    joint2 (x:y:zs) = (Joint x y :) <$> joint2 zs
    joint2 [ ] = Just []
    joint2 [_] = Nothing
    joint :: [Tree] -> Maybe Tree
    joint [ ] = Nothing
    joint [x] = Just x
    joint xs  = joint2 xs >>= joint

visit :: Tree -> [Bool] -> Maybe Bool
visit (Node b) [] = Just b
visit (Joint f t) (x:xs) = visit (if x then t else f) xs
visit _ _ = Nothing

depth :: Tree -> Int
depth (Node _) = 0
depth (Joint f _) = succ (depth f) -- (depth f == depth t) by construction
