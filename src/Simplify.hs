module Simplify (simplify) where

import AST
import Operators
import Values
import Polynomial
import Prelude hiding (exponent)
import Data.List (sort)

simplify :: SyntaxTree Value Operator -> SyntaxTree Value Operator
simplify = collect . filter (\(Monomial (a, _)) -> a /= 0 ) . pSort . polynomize . expandNL

expandNL :: SyntaxTree Value Operator -> SyntaxTree Value Operator

expandNL (Branch Power base exponent) = case simplify exponent of
    n@(Leaf (Constant _)) -> Branch Power base n
    x          -> Branch Multiply base $ expfactors x
        where expfactors (Branch Plus y z) = Branch Multiply (expfactors y) (expfactors z)
              expfactors tree              = Leaf $ Apply Exp tree

expandNL (Branch op left right) = Branch op (expandNL left) (expandNL right)

expandNL (Leaf (Apply Ln arg)) = lnterms $ simplify arg
    where lnterms (Branch Multiply y z) = Branch Plus (lnterms y) (lnterms z)
          lnterms tree                  = Leaf $ Apply Ln tree
    
expandNL (Leaf (Apply unary arg)) = Leaf $ Apply unary $ simplify arg

expandNL x = x

pSort :: Polynomial Double (SyntaxTree Value Operator) -> Polynomial Double (SyntaxTree Value Operator)
pSort = sort . pNormalize

collect :: Polynomial Double (SyntaxTree Value Operator) -> SyntaxTree Value Operator
collect = foldr1 (Branch Plus) . fmap monomialToBranch
    where
        monomialToBranch :: Monomial Double (SyntaxTree Value Operator) -> SyntaxTree Value Operator
        monomialToBranch (Monomial (a, xs)) = Branch Multiply (Leaf (Constant a)) $ foldr1 (Branch Multiply) $ fmap go xs
        go :: (SyntaxTree Value Operator, Double) -> SyntaxTree Value Operator
        go (x, 1) = x
        go (x, n) = Branch Power x $ Leaf $ Constant n
