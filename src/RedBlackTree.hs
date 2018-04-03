module RedBlackTree where

data Color = R | B deriving (Show)

data Tree a = Leaf 
            | Branch Color (Tree a) a (Tree a) deriving (Show)

member :: (Ord a) => a -> Tree a -> Bool
member _ Leaf = False
member a (Branch  _ right v left)
    | a == v = True
    | a < v = member a left
    | a > v = member a right

insert :: (Ord a) => a -> Tree a -> Tree a
insert a node = makeBlack $ insert' a node
    where 
        insert' :: (Ord a) =>  a -> Tree a -> Tree a
        insert' a Leaf = Branch R Leaf a Leaf
        insert' a node@(Branch c l v r)
            | a == v = node
            | a < v = balance c (insert' a l) v r
            | a > v = balance c l v (insert' a r)

balance :: Color -> Tree a  -> a -> Tree a -> Tree a
balance B (Branch R (Branch R a x b) y c) z d = Branch R (Branch B a x b) y (Branch B c z d)
balance B (Branch R a x (Branch R b y c)) z d = Branch R (Branch B a x b) y (Branch B c z d)
balance B a x (Branch R (Branch R b y c) z d) = Branch R (Branch B a x b) y (Branch B c z d)
balance B a x (Branch R b y (Branch R c z d)) = Branch R (Branch B a x b) y (Branch B c z d)
balance c n1 v n2 = (Branch c n1 v n2)

makeBlack :: Tree a -> Tree a
makeBlack (Branch _ r v l) = Branch B r v l
