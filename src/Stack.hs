module Stack where 

data Stack a = Nil | Cons a (Stack a) deriving (Show, Read, Eq)

top :: Stack a -> a
top Nil = error "Nil has no head"
top (Cons a _)  = a

pop :: Stack a -> Stack a
pop Nil = Nil
pop (Cons a xs) = xs

isEmpty :: Stack a -> Bool
isEmpty Nil = True
isEmpty (Cons _ _) = False

push :: a -> Stack a -> Stack a
push a xs = Cons a xs

-- violation of the stack just because I can :|
contains :: (Eq a) => a -> Stack a -> Bool
contains _ Nil = False
contains y (Cons x xs) = if y == x then True else contains y xs

find :: (Eq a) => (a -> Bool) -> Stack a -> Stack a
find f Nil = Nil
find f (Cons x xs) = if f x then Cons x $ find f xs else find f xs

{--
    - how do I use this. Well this is just a play thing, so you can play with it by loading it in ghci :l src/Stack.hs
    - pop Nil => brow up!!
    - pop Cons 1 $ Nil => Nil
--}
