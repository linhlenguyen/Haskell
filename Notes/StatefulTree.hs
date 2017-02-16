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
      traverse f (Node a lhs rhs) = Node <$> (f a) <*> traverse f lhs <*> traverse f rhs -- pre Order

    -- (a -> b -> c) <$> t a <*> t b

    preOrderFold :: (a -> b -> b) -> b -> Tree a -> b
    preOrderFold f acc Empty = acc
    preOrderFold f acc (Node a lhs rhs) = preOrderFold f (preOrderFold f (f a acc) lhs) rhs

    inOrderFold :: (a -> b -> b) -> b -> Tree a -> b
    inOrderFold f acc Empty = acc
    inOrderFold f acc (Node a lhs rhs) = inOrderFold f (f a (inOrderFold f acc lhs)) rhs

    postOrderFold :: (a -> b -> b) -> b -> Tree a -> b
    postOrderFold f acc Empty = acc
    postOrderFold f acc (Node a lhs rhs) = f a (postOrderFold f (postOrderFold f acc lhs) rhs)

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
    testTree = Prelude.foldr (addNode) Empty ['f','d','b','a','c','e','h','g','i']

    testTree' :: Tree Int
    testTree' = Prelude.foldr (addNode) Empty [5,7,6,2,1,3,8]

    newTree :: (Ord a) => [a] -> Tree a
    newTree ls = Prelude.foldr (addNode) Empty ls

    addIndex :: Tree a -> Tree (Int, a)
    addIndex t = runST $ do
      i <- newSTRef 0
      mapM (\x -> do
             i' <- readSTRef i
             writeSTRef i (i'+1)
             return (i', x)
             ) t

    data Box = forall a. (Show a) => Box a

    instance Eq Box where
      (Box a) == (Box b) = False

    instance Ord Box where
      (Box a) <= (Box b) = False

    data Tree' = TEmpty | TNode Box Tree' Tree'

    instance Show Tree' where
      show TEmpty = "Nothing"
      show (TNode (Box a) l r) = " " ++ show a ++ " " ++ show l ++ " " ++ show r

    tMap :: (Ord b, Show b) => (Box -> b) -> Tree' -> Tree'
    tMap f TEmpty = TEmpty
    tMap f (TNode a lhs rhs) = (TNode (Box (f a)) (tMap f lhs) (tMap f rhs))

    addNodeT' :: Box -> Tree' -> Tree'
    addNodeT' x TEmpty = TNode x TEmpty TEmpty
    addNodeT' x (TNode a l r) = if (x <= a) then (TNode a (addNodeT' x l) r) else (TNode a l (addNodeT' x r))

    newTreeT' :: Tree'
    newTreeT' = Prelude.foldr (\x t -> addNodeT' x t) TEmpty [Box 5, Box 3, Box 'a', Box 'b', Box 1]

    --addIndexls :: [a] -> [(Int, a)]
    addIndexls ls = runST $ do
      i <- newSTRef 0
      traverse (\x -> do
          i' <- readSTRef i
          writeSTRef i (i'+1)
          return (i',x)) ls

    --map :: (a -> m b) -> a -> [m b]
    --lift :: [m b] -> m [b]
    --lift xs = return (map (runST) xs)

    main :: IO ()
    main = putStrLn (show testTree)
