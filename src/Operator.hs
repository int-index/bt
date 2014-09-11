{-# LANGUAGE LambdaCase #-}
module Operator where

import qualified Data.Map as M

type Operators = M.Map String Operator

data Fixity1 = Prefix  | Postfix  deriving (Eq)
data Fixity2 = Leftfix | Rightfix | Nonfix deriving (Eq)

data Operator = BinaryOperator  [String] (Int, Fixity2)
              | UnaryOperator   [String] (Int, Fixity1)
              | NullaryOperator [String]

aliases :: Operator -> [String]
aliases = \case
    BinaryOperator  as _ -> as
    UnaryOperator   as _ -> as
    NullaryOperator as   -> as

operators :: Operators
operators = M.fromList
    [ ("true" , NullaryOperator ["1"])
    , ("false", NullaryOperator ["0"])
    , ("id"  , UnaryOperator   ["#"] (1, Postfix))
    , ("not" , UnaryOperator   ["-"] (2, Prefix))
    , ("and" , BinaryOperator  ["&"] (4, Leftfix))
    , ("or"  , BinaryOperator  ["|"] (6, Leftfix))
    , ("xor" , BinaryOperator  ["+"] (5, Leftfix))
    , ("nand", BinaryOperator  ["↑", "-&"]  (4, Leftfix))
    , ("nor" , BinaryOperator  ["↓", "-|"]  (6, Leftfix))
    , ("ent" , BinaryOperator  ["→", "->" ] (7, Rightfix))
    , ("equ" , BinaryOperator  ["~"] (8, Nonfix))
    ]
