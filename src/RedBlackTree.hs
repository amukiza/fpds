module RedBlackTree where

data Color = R | B deriving (Show, Eq, Ord)

data Tree a = Leaf | Node Color (Tree a) a (Tree a) deriving (Show, Eq, Ord)

member :: (Ord a) => a -> Tree a -> Bool
member _ Leaf = False
member a (Node _ r v l)
    | a == v = True
    | a < v = member a r
    | a > v = member a l
