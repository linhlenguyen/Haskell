{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Boolean where

import Prop
import BddIO

-- Some class definitions and some instances 
-- Something acts like a Boolean if it supports these
-- operations

class (Monad m,Show b) => Boolean m b  where
  true :: m b
  false :: m b
  isTrue :: b -> m Bool
  isFalse :: b -> m Bool
  conj:: b -> b -> m b     -- conjunction
  disj:: b -> b -> m b     -- disjunction
  neg:: b -> m b           -- negation
  imply:: b -> b -> m b    -- implication
  

instance Monad m => Boolean m Bool where
  true = return True
  false = return False
  isTrue x = return x
  isFalse = return . not
  conj x y = return(x && y)
  disj x y = return(x || y)
  neg = return . not
  imply x y = return(not x || y)
  
instance (Monad m,PPLetter n,Ord n) => Boolean m (Prop n) where
  true =  return TruthP
  false = return AbsurdP
  isTrue TruthP = return True
  isTrue x = return False
  isFalse AbsurdP = return True
  isFalse x = return False
  conj x y = return(andB x y)
  disj x y = return(orB x y)
  neg = return . notB
  imply x y = return(orB (notB x) y)  
  
instance Boolean IO (Bdd Int) where
  true = return trueBdd
  false = return falseBdd
  isTrue x = return (x==trueBdd)
  isFalse x = return(x==falseBdd)
  conj x y = andBdd x y
  disj x y = orBdd x y
  neg x = notBdd x
  imply x y = implyBdd x y
 
