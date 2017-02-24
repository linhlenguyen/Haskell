{-# LANGUAGE GADTs #-}

import Control.Monad

data Tree a where
  Empty :: Tree a
  Node :: a -> [Tree a] -> Tree a
  ID :: Int -> Tree a -> Tree a
 deriving (Show)

newtype State s a = State { statef :: s -> (a,s) }

-- f a -> (a - b) = f b
instance Functor (State s) where
  fmap f sf = State $ \s -> let (a,si) = (statef sf) s in
                            (f a, s)

-- f a -> f (a -> b) = f b
-- (State s) a -> (State s) (a -> b) -> (State s) b
-- (State s) (a -> b) ~ State $ \s -> ((a -> b),s)
instance Applicative (State s) where
  pure a = State $ \s -> (a,s)
  fa <*> si = State $ \s -> let (a,sa) = (statef si) s
                                (f,sf) = (statef fa) sa in
                                (f a, sa)

-- S a -> ( a -> S b ) -> S b
-- \sa -> (a,sa) -> (a -> (\sb -> (b,sb))) -> (\sb -> (b, sb))
instance Monad (State s) where
  return a = State $ \s -> (a,s)
  si >>= f = State $ \s -> let (a,sa) = statef si s
                               (b,sb) = statef (f a) sa in
                               (b,sb)

sampleTree :: Tree a
sampleTree = Node 2 [Empty, Node 1 [], Node 4 [Node 5 [],Empty, Node 6 []]]

mapID :: Tree a -> Tree a
mapID = undefined
