{-# LANGUAGE FlexibleInstances #-}
module Boolean.Reflection where

import Control.Applicative

import Boolean.Expression
import qualified Data.Boolean.Tree as T

tree = Tree . T.unsafeFromList

argument :: [Bool]
argument = [False, True]

reflect0 :: Bool -> Function
reflect0 f = tree $ pure f

reflect1 :: (Bool -> Bool) -> Function
reflect1 f = tree $ pure f <*> argument

reflect2 :: (Bool -> Bool -> Bool) -> Function
reflect2 f = tree $ pure f <*> argument <*> argument
