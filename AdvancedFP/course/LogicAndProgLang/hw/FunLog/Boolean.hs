{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Boolean where

-- The idea is to abstract over types that act like Bool.
-- Something acts like a Boolean if it supports a certain
-- set of operations. We break these operations
-- into 3 classes.

-- TPD means Two Point Domain, the classic example is Bool
class Show b => TPD b where
  true :: b
  false :: b
  isTrue :: b -> Bool
  isFalse :: b -> Bool

class (TPD b) => Boolean b  where
  conj:: b -> b -> b     -- conjunction
  disj:: b -> b -> b     -- disjunction
  neg:: b -> b           -- negation
  imply:: b -> b -> b    -- implication
  xor:: b -> b -> b      -- exclusive Or
  equal:: b -> b -> b    -- if and only if
  -- default implementations
  imply x y = disj(neg x) y
  xor x y = neg(equal x y)
  equal x y = conj (imply x y) (imply y x)
  
class (Monad m,TPD b) => BooleanM m b  where
  conjM:: b -> b -> m b     -- conjunction
  disjM:: b -> b -> m b     -- disjunction
  negM:: b -> m b           -- negation
  implyM:: b -> b -> m b    -- implication  
  xorM:: b -> b -> m b      -- exclusive or
  equalM:: b -> b -> m b    -- if and only if  
  
  equalM x y = do { z <- xorM x y; negM z}
  
--------------------------------------------------
-- now some instances

instance TPD Bool where
  true = True
  false = False
  isTrue x = x
  isFalse = not
  
instance Boolean Bool where
  conj x y = (x && y)
  disj x y = (x || y)
  neg = not
  imply x y = imp x y
  xor x y = exOr x y
  equal x y = sameB x y

imp False x = True
imp True x = x

exOr True False = True
exOr False True = True
exOr _ _ = False

sameB True True = True
sameB False False = True
sameB _ _ = False

instance Monad m => BooleanM m Bool where
  conjM x y = return(conj x y)
  disjM x y = return(disj x y)
  negM x = return(neg x)
  implyM x y = return(disj (neg x) y)
  xorM x y = return(xor x y)
  equalM x y = return(equal x y)


