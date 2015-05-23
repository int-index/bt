module Boolean.Operator where

import Boolean.Expression
import qualified Data.Map as M

type Operators = M.Map Name Operator

data Fixity1 = Prefix  | Postfix  deriving (Eq)
data Fixity2 = Leftfix | Rightfix | Nonfix deriving (Eq)

data Operator = BinaryOperator  [String] (Int, Fixity2)
              | UnaryOperator   [String] (Int, Fixity1)
              | NullaryOperator [String]
