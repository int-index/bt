module Boolean.Predef where

import Boolean.Expression
import Boolean.Operator

import qualified Data.Boolean.Tree as T

import qualified Data.Map as M

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

functions :: Definitions
functions = M.fromList
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
    ] where tree = Tree . T.unsafeFromList
