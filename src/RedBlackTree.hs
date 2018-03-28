module RedBlackTree where

data Color = R | B deriving (Show, Eq, Ord)

data Tree a = Leaf 
            | Branch Color (Tree a) a (Tree a) deriving (Show, Eq, Ord)

member :: (Ord a) => a -> Tree a -> Bool
member _ Leaf = False
member a (Branch  _ right v left)
    | a == v = True
    | a < v = member a left
    | a > v = member a right

insert :: (Ord a) => a -> Tree a -> Tree a
insert a Leaf = Branch B Leaf a Leaf
insert a node@(Branch color right v left)
    | a == v = node
    | a < v = balance (insert a left) right
    | a > v = balance (insert a right) left

balance :: (Ord a) => Tree a -> Tree a -> Tree a
balance = undefined
