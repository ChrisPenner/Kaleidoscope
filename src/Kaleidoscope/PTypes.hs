module Kaleidoscope.PTypes where

import qualified Data.Text as T

type Name = T.Text

data Expr =
    Float Double
  | BinOp Op Expr Expr
  | Var T.Text
  | Call Name [Expr]
  | Function Name [Expr] Expr
  | Extern Name [Expr]
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)
