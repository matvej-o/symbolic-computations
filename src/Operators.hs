module Operators (Operator(..), oprint) where

data Operator = Plus
              | Multiply
              | Power deriving (Eq, Show)


oprint :: Operator -> String
oprint Plus     = " + "
oprint Multiply = " "
oprint Power    = "^"