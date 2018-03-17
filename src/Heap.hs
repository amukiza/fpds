module Heap where

data Heap a = Empty | Node Int a (Heap a) (Heap a) deriving (Show, Eq)

empty :: Heap a
empty = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty (Node _ _ _ _) = False

findMin :: (Ord a) => Heap a -> a
findMin Empty = error "empty heap"
findMin (Node _ e _ _) = e

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin (Node _ v r l) = merge r l

insert :: (Ord a) => a -> Heap a -> Heap a
insert v h = merge (Node first v Empty Empty) h 
    where first = 1

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty n  = n
merge n Empty  = n
merge h1@(Node _ a r1 l1) h2@(Node _ b r2 l2)
    | a <= b = makeT a r1 $ merge r1 h2
    | otherwise = makeT b r2 $ merge h1 l2
    where
        makeT :: a -> Heap a -> Heap a -> Heap a
        makeT v r l
            | r1 >= r2 = (Node (succ r2) v r l)
            | otherwise =  (Node (succ r1) v l r)
            where
                r1 = rank r
                r2 = rank l
        rank Empty = 0
        rank (Node r _ _ _) = r
