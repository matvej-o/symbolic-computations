module AST (SyntaxTree(..), ZipperDirection, moveZip, fromZipper, updateZipper) where

data SyntaxTree v op = Leaf v
                     | Branch op (SyntaxTree v op) (SyntaxTree v op) deriving (Eq, Show)

data ZipperDirection = MoveLeft
                     | MoveRight
                     | MoveBack deriving Show
                     -- | MoveThrough deriving Show

instance (Ord v, Eq op) => Ord (SyntaxTree v op) where
    (Branch _ t1 s1) <= (Branch _ t2 s2) = t1 <= t2 || (t1 == t2 && s1 <= s2) -- left ordering
    (Branch _ t1 _) <= (Leaf b)           = t1 <= Leaf b
    (Leaf a) <= (Branch _ t2 _)           = Leaf a <= t2
    (Leaf a) <= (Leaf b)                  = a <= b

data Zipper v op = Zipper {getZipLine :: ZipLine v op, fromZipper :: SyntaxTree v op} deriving Show

data Zip v op = ZipLeft op (SyntaxTree v op)
              | ZipRight op (SyntaxTree v op) deriving Show
              -- | ZipThrough op deriving Show

type ZipLine v op = [Zip v op]

moveZip :: ZipperDirection -> Zipper v op -> Maybe (Zipper v op)
moveZip MoveLeft (Zipper zipLine (Branch f left right)) = Just $ Zipper ((ZipLeft f right) : zipLine) left
moveZip MoveRight (Zipper zipLine (Branch f left right)) = Just $ Zipper ((ZipRight f left) : zipLine) right
--moveZip MoveThrough (Zipper zipLine (BranchThrough f through)) = Just $ Zipper ((ZipThrough f) : zipLine) through
moveZip MoveBack (Zipper ((ZipLeft f right) : zipline) tree) = Just $ Zipper zipline $ Branch f tree right
moveZip MoveBack (Zipper ((ZipRight f left) : zipline) tree) = Just $ Zipper zipline $ Branch f left tree
--moveZip MoveBack (Zipper ((ZipThrough f) : zipline) tree) = Just $ Zipper zipline $ BranchThrough f tree
moveZip _     _                                          = Nothing

updateZipper :: Zipper v op -> SyntaxTree v op -> Zipper v op
updateZipper zipper tree = zipper {fromZipper = tree}