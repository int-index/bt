module Data.Boolean.Tree where

import Data.Maybe
import Control.Applicative

data Tree a = Joint (Tree a) (Tree a) | Node a

instance Functor Tree where
    fmap h (Node a) = Node (h a)
    fmap h (Joint f t) = Joint (fmap h f) (fmap h t)

toList :: Tree a -> [a]
toList (Node a) = [a]
toList (Joint f t) = toList f ++ toList t

unsafeFromList :: [a] -> Tree a
unsafeFromList = fromJust . fromList

fromList :: [a] -> Maybe (Tree a)
fromList = joint . map Node
  where
    joint2 :: [Tree a] -> Maybe [Tree a]
    joint2 (x:y:zs) = (Joint x y :) <$> joint2 zs
    joint2 [ ] = Just []
    joint2  _  = Nothing
    joint :: [Tree a] -> Maybe (Tree a)
    joint [ ] = Nothing
    joint [x] = Just x
    joint xs  = joint2 xs >>= joint

visit :: Tree a -> [Bool] -> Maybe a
visit (Node a) [] = Just a
visit (Joint f t) (x:xs) = visit (if x then t else f) xs
visit _ _ = Nothing

depth :: Tree a -> Int
depth (Node _) = 0
depth (Joint f _) = succ (depth f) -- (depth f == depth t) by construction
