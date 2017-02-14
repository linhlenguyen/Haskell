{-# LANGUAGE GADTs, ExistentialQuantification #-}

module StatefulTree(

)
  where
    import Control.Monad.ST
    import Data.STRef
    import Control.Monad
    import Data.Functor
    import Data.Foldable
    import Data.Traversable

    data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

    instance Functor Tree where
      fmap f Empty = Empty
      fmap f (Node a lhs rhs) = (Node (f a) (fmap f lhs) (fmap f rhs))

    instance Foldable Tree where
      foldr f acc Empty = acc
      foldr f acc (Node a lhs rhs) = foldr f (f a (foldr f acc rhs)) lhs

    instance Traversable Tree where
      --traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
      traverse f Empty = pure Empty
      traverse f (Node a lhs rhs) = Node <$> (f a) <*> traverse f lhs <*> traverse f rhs

    addNode' :: (Ord a) => Tree a -> a -> Tree a
    addNode' t a = addNode a t

    addNode :: (Ord a) => a -> Tree a -> Tree a
    addNode a Empty = Node a Empty Empty
    addNode a (Node b lhs rhs) = if a <= b then (Node b (addNode a lhs) rhs) else (Node b lhs (addNode a rhs))

    foldf :: (Ord a) => a -> Tree (a, a) -> Tree (a, a)
    foldf a tb = addNode (a, a) tb

    foldi :: (Ord a) => a -> (Int, Tree (Int, a)) -> (Int, Tree (Int, a))
    foldi a (i, t) = (i+1, addNode (i, a) t)

    mfoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
    mfoldr = undefined

    testTree :: Tree Char
    testTree = Prelude.foldl (\x t -> addNode' x t) Empty ['f','d','b','a','c','e','h','g','i']

    testTree' :: Tree Int
    testTree' = Prelude.foldl (\x t -> addNode' x t) Empty [5,7,6,2,1,3,8]

    addIndex :: Tree a -> Tree (Int, a)
    addIndex t = runST $ do
      i <- newSTRef 0
      mapM (\x -> do
             i' <- readSTRef i
             writeSTRef i (i'+1)
             return (i', x)
             ) t

    main :: IO ()
    main = putStrLn (show testTree)
