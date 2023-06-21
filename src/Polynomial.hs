module Polynomial where

import AST
import Operators
import Values
import Data.List (sortBy)

newtype Monomial coeff var = Monomial (coeff, [(var, Double)]) deriving (Show, Eq)

instance (Ord var, Eq coeff) => Ord (Monomial coeff var) where
    compare (Monomial (a, (x, n) : xs)) (Monomial (b, (y, m) : ys)) = compare x y <> compare n m <> compare (Monomial (a, xs)) (Monomial (b, ys))
    compare (Monomial (_, [])) (Monomial (_, _ : _)) = LT
    compare (Monomial (_, _ : _)) (Monomial (_, [])) = GT
    compare (Monomial (_, [])) (Monomial (_, [])) = EQ


type Polynomial coeff var = [Monomial coeff var]

polynomize :: SyntaxTree Value Operator -> [Monomial Double (SyntaxTree Value Operator)]

polynomize (Branch Multiply (Leaf (Constant a)) (Leaf (Constant b))) = [Monomial (a * b, [])]
polynomize (Branch Plus (Leaf (Constant a)) (Leaf (Constant b))) = [Monomial (a * b, [])]
polynomize (Branch Power (Leaf (Constant a)) (Leaf (Constant b))) = [Monomial (a ** b, [])]

polynomize (Branch Multiply (Leaf (Constant a)) unknown@(Leaf _)) = [Monomial (a, [(unknown, 1)])]
polynomize (Branch Multiply unknown@(Leaf _) (Leaf (Constant a))) = [Monomial (a, [(unknown, 1)])]

{--
polynomize (Branch Multiply (Branch Plus a b) (Branch Plus c d)) = ac <+> ad <+> bc <+> bd
    where ac = polynomize (Branch Multiply a c)
          ad = polynomize (Branch Multiply a d)
          bc = polynomize (Branch Multiply b c)
          bd = polynomize (Branch Multiply b d)

--}

polynomize (Branch Multiply t1 t2) = polynomize t1 `pMult` polynomize t2
polynomize (Branch Plus t1 t2) = polynomize t1 <+> polynomize t2

polynomize (Branch Power a (Leaf ~(Constant n))) = pPower (polynomize a) $ round n
polynomize (Branch Power _ _) = error "Nonlinear expansion error: this place should never be reached."

polynomize (Leaf a) = [Monomial (1, [(Leaf a, 1)])]


scale :: Num b => b -> Monomial b a -> Monomial b a
scale n (Monomial (a, x)) = Monomial (n * a, x)

pNormalize :: (Ord a) => Polynomial b a -> Polynomial b a
pNormalize = fmap mNormalize
    where mNormalize (Monomial (coeff, unknowns)) = Monomial (coeff, sortBy mOrder unknowns)
          mOrder (x, n) (y, m)
            | x <= y = LT
            | x == y && n >= m = LT
            | otherwise = GT

(<+>) :: (Ord a, Num b) => [Monomial b a] -> [Monomial b a] -> [Monomial b a]
p1 <+> p2 = pNormalize $ go p1 p2 []
    where
        go p@((Monomial (a, x)) : xs) ((Monomial (b, y)) : ys) yss
            | x == y = Monomial (a + b, x) : go xs (yss ++ ys) []
            | otherwise = go p ys (Monomial (b, y) : yss)
        go (x : xs) [] yss = x : (go xs yss [])
        go [] ys yss = ys ++ yss

pMult :: (Ord a, Num b) => Polynomial b a -> Polynomial b a -> Polynomial b a
pMult p1 p2 = pNormalize $ go p1 p2
    where
        go (x : xs) q = fmap (mMult x) q <+> go xs q
        go [] q = []
        
mMult :: (Ord a, Num b) => Monomial b a -> Monomial b a -> Monomial b a
mMult (Monomial (a, xs)) (Monomial (b, ys)) = Monomial (a * b, go xs ys []) 
    where
        go :: (Ord a, Num b) => [(a, b)] -> [(a, b)] -> [(a, b)] -> [(a, b)]
        go ((x, n) : xs) ((y, m) : ys) yss
            | x == y = (x, n + m) : go xs (ys ++ yss) []
            | otherwise = go ((x, n) : xs) ys ((y, m) : yss)
        go (x : xs) [] yss = x : (go xs yss [])
        go [] ys yss = ys ++ yss

binom :: Int -> Int -> Int
binom n k = (triangle !! n) !! k

triangle :: [[Int]]
triangle = iterate nextRow [1]
  where nextRow xs = 1 : zipWith (+) xs (tail xs) ++ [1]


pPower :: (Ord a) => Polynomial Double a -> Int -> Polynomial Double a
pPower [] _ = []
pPower [Monomial (a, unknowns)] n = [Monomial (a ^ n, flip fmap unknowns $ \(x, m) -> (x, m + fromIntegral n))]
pPower (p : ps) n = foldr1 (<+>) $ flip fmap [1..n] $ \k -> (scale (fromIntegral (binom n k))) <$> ((pPower [p] n) `pMult` (pPower ps n))

