module ListZipper (
List(..)
ListZipper,
foward,
backward
)
where

  data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

  type ListZipper a = (List a, List a)

  foward :: ListZipper a -> ListZipper a
  foward (Cons a la, lb) = (lsa, Cons a lb)

  backward :: ListZipper a -> ListZipper a
  backward (la, Cons b lb) = (Cons b la, lb)

  type ListZipper' a = ([a], [a])

  foward :: ListZipper' a -> ListZipper' a
  foward (xs, []) = (xs,[])
  foward (prev, x:xs) = (x:prev, xs)

  backward :: ListZipper' a -> ListZipper' a
  backward ([], xs) = ([], xs)
  backward (x:xs, next) = (xs, x:next)
