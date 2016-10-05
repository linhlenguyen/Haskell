class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

(<$>) :: (Functor f) -> (a -> b) -> f a -> f b
f <$> x = fmap f x

liftA :: (Applicative f) -> (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA2 :: (Applicative f) -> (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b c = fmap f a <*> b
