module Values (Value(..), Unary(..)) where

import AST
import Operators

data Value = Constant Double
           | Variable Char
           | Apply Unary (SyntaxTree Value Operator) deriving (Eq)
           
instance Show Value where
           show (Constant a)
                | a > 0 = show a
                |otherwise = "(" ++ show a ++ ")"
           show (Variable x) = [x]
           show (Apply unary tree) = show unary ++ "(" ++ show tree ++ ")"
           
data Unary = Sin
           | Cos
           | Tg
           | Arcsin
           | Arccos
           | Arctg
           | Exp
           | Ln
           | Sh
           | Ch
           | Th deriving (Show, Eq, Ord)
           
instance (Ord Value) where
    Constant _ <= Variable _ = True
    Constant a <= Constant b = a <= b
    Variable x <= Variable y = x <= y
    Constant _ <= Apply _ _ = True
    Variable _ <= Apply _ _ = True
    Apply unary1 t1 <= Apply unary2 t2 = (unary1 <= unary2) || (unary1 == unary2 && t1 <= t2) 