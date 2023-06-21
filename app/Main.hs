module Main (main) where

import AST
import Operators
import Values
import Simplify

main :: IO ()
main = do
    putStrLn "Here is a simple example:"
    let expr = Branch Multiply (Branch Plus a b) (Branch Plus a (Branch Multiply (cnst (-1)) b))
    pprint expr
    print expr
    --putStrLn "Branch Multiply (Branch Plus a b) (Branch Plus a (Branch Multiply (cnst (-1)) b))"
    putStrLn "After simplification it turns into: "
    let sExpr = simplify expr
    pprint sExpr
    print sExpr
    putStrLn "You may explore further with GHCI. Use 'pprint' for pretty-printing and 'print' to print the abstract syntax tree."

a = Leaf (Variable 'a')
b = Leaf (Variable 'b')
c = Leaf (Variable 'c')
x = Leaf (Variable 'x')
y = Leaf (Variable 'y')
t = Leaf (Variable 't')
cnst = Leaf . Constant

pprint :: SyntaxTree Value Operator -> IO ()
pprint = putStrLn . pshow

pshow :: SyntaxTree Value Operator -> String
pshow (Leaf v) = show v
pshow (Branch Multiply left@(Branch Plus _ _) right@(Branch Plus _ _)) = "(" ++ pshow left ++ ")(" ++ pshow right ++ ")"
pshow (Branch op left right) = pshow left ++ oprint op ++ pshow right