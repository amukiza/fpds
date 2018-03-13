module Tree where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

singleton :: a -> Tree a
singleton a = Node a Empty Empty 
 
member :: (Ord a) => a -> Tree a -> Bool
member a Empty = False
member a (Node x l r)
   | a == x = True
   | a <  x = member a l
   | a >  x = member a r

insert :: (Ord a) => a -> Tree a -> Tree a
insert a Empty = singleton a
insert a (Node x l r)
    | a == x = Node x l r
    | a <  x = Node x (insert a l) r
    | a >  x = Node x l (insert a r)
