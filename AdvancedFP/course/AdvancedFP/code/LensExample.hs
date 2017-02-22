{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module LensExample where

import Control.Lens
import Control.Lens.TH

------------------------------------------------------------
-- Fields as Lenses.  A field is an index into a tuple of
-- finite width. Fields are defined for tuples upto witdth 9

--  (^.) is the getter function. Its normal prefix function mae is "view"

tuple = (1,True,'c',23.4,"abc",Left 5,Just 1)

x3 = tuple ^. _3
x2 = tuple ^. _2
x7 = tuple ^. _7
x4 = view _4 tuple
-- x8 = tuple ^. _8    -- raises a type error.

-- set is the setter

s1 = set _4 0.0 tuple
-- *LensExample> s1
-- (1,True,'c',0.0,"abc",Left 5,Just 1)

-- one can compose fields using ordinary composition (.) from the prelude

s2 = set (_2 . _2) 9 ("A",(True,'x'))
-- *LensExample> s2
-- ("A",(True,9))

-- An ordinary (unary) function can be made into a getter using "to"
-- *LensExample> "abc" ^. (to length)
-- 3
-- *LensExample> (True,"abc",34)  ^. (_2. to length)
-- 3
-- *LensExample> "abcd" ^. (to length)
-- 4
-- *LensExample> (2,"abcd",True) ^. (_2 . to length)
-- 4
-- *LensExample> (2,"abcd",(True,[1])) ^. (_3 . _2 . to length)
-- 1

ith n = to (!! n)

-- (.~) is an alias for set

s4 = ((_2 . _1) .~ 99) (6,(True,"abc"))
-- *LensExample> s4
-- (6,(99,"abc"))

-- Making and using lenses for user defined types.

data Person = P { _name:: String
                , _age :: Int
                , _married:: Bool
                , _offspring:: [Person] } deriving Show
                
makeLenses ''Person

tom = P "Tom" 34 True []
bob = P "Bob" 9 False []
ann = P "Ann" 5 False []



s5 = set offspring [bob,ann] tom
s6 = s5 ^. offspring . (ith 1) . name

nth 0 = _head
nth n = _tail . (nth (n-1))

s7 = set (nth 3) 99 [1,2,3,4,5,6]
s8 = set (offspring . (nth 1) . name) "mary" s5

-- nchild :: Functor f => (Int -> f Int) -> Person -> f Person
nchild:: Lens Person Person Int Int
nchild g (P n ag m off) = 
   fmap (\ z -> P n ag m 
            (case z of 
              m | m==length off -> off
              m | m < length off -> take m off
              m | m > length off -> take m (off ++ (repeat ann))))
        (g (length off))


ageAll:: Lens Person Person [Int] [Int]
ageAll g (P n ag m off) = 
  fmap (\ (ag:ags) -> P n ag m (zipWith (set age) ags off))
       (g (ag: map (view age) off))
----------------------------------------------------------