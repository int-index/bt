module Boolean.Predef where

import Boolean.Expression
import Boolean.Operator
import Boolean.Reflection

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
    [ ("true" , reflect0 True )
    , ("false", reflect0 False)
    , ("id"  , reflect1 id  )
    , ("not" , reflect1 not )
    , ("and" , reflect2 (&&))
    , ("or"  , reflect2 (||))
    , ("xor" , reflect2 (/=))
    , ("nand", reflect2 nand)
    , ("nor" , reflect2 nor )
    , ("ent" , reflect2 ent )
    , ("equ" , reflect2 (==))
    ] where nand x y = not (x && y)
            nor  x y = not (x || y)
            ent  x y = not  x || y
