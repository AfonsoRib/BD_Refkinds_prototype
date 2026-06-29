module Constraint where

import Types as T

data Cstr
    = CPred T.Pred
    | CAnd [Cstr]
    | CAll Bind Cstr
    deriving (Show, Eq)

data Bind = Bind T.Identifier T.BaseKind T.Pred
    deriving (Show, Eq)

ctrue :: Cstr
ctrue = CAnd []