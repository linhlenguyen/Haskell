{-# Language ScopedTypeVariables #-}

module Main where

import Prelude hiding ( (<*) )
import SOE(Event(..))
import Shape
import Region
import Fal hiding (color1,color2,color3,color4)
import Animation (main1,main2,main3,main4,main5,main6,main7,main8)
 
main = 
  do putStrLn ("Enter demo number\n")
     (n::Int) <- readLn
     case n of
       0 -> test (paddleball 3)
       1 -> main1
       2 -> main2
       3 -> main3
       4 -> main4
       5 -> main5
       6 -> main6
       7 -> main7
       8 -> main8
       

dot = (ell 0.2 0.2)
ex1 = paint red (translate (0, time / 2) dot)

ex2 = paint blue (translate (sin time,cos time) dot)

wander x y color = paint color (translate (x,y) dot)

ex3 = wander (time /2) (sin time) red

modula x y = (period,w)
    where (whole,fract) = properFraction x
          n = whole `mod` y
          period = (whole `div` y)
          w = (fromIntegral (toInteger n)) + fract
          
bounce t = f fraction
   where (period,fraction) = modula t 2
         f = funs !! (period `mod` 4)
         funs = [id,(2.0 -),negate,(\x -> x - 2.0)] 


ex4 = wander (lift1 bounce time) 0 yellow     

color0 = red `switch` (lbp ->> blue)
moon = (translate (sin time,cos time) dot)
ex5 = paint color0 moon

color1 = red `switch` 
              (lbp `withElem_` cycle [blue,red])

ex6 = paint color1 moon




color2 = red `switch` 
             ((lbp ->> blue) .|. (key ->> yellow))

ex7 = paint color2 moon

color3 = white `switch` (key =>> \c ->
           case c of 'r' -> red
                     'b' -> blue
                     'y' -> yellow 
                     _   -> white  )
ex8 = paint color3 moon

color4 = white `switch` ((key `snapshot` color4) =>> \(c,old) ->
           case c of 'r' -> red
                     'b' -> blue
                     'y' -> yellow 
                     _   -> constB old)
ex9 = paint color4 moon



size '2' = 0.2  -- size :: Char -> Float
size '3' = 0.4
size '4' = 0.6
size '5' = 0.8
size '6' = 1.0
size '7' = 1.2
size '8' = 1.4
size '9' = 1.6
size _ = 0.1

growCircle :: Char -> Region
growCircle x = Shape(Ellipse (size x) (size x))

ex10 =  paint red (Shape(Ellipse 1 1) 
                     `step` (key =>> growCircle))


rbp :: Fal.Event ()
rbp = Event (\(uas,_) -> map getrbp uas)
      where getrbp (Just (Button _ False True)) = Just ()
            getrbp _                           = Nothing

power2 :: Fal.Event(Float -> Float)
power2 = (lbp ->> \ x -> x*2)     .|. 
         (rbp ->> \ x -> x * 0.5)

dynSize = 1.0 `stepAccum` power2
ex11 = paint red (ell dynSize dynSize)

(>=*),(<=*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
(>=*) = lift2 (>=)
(<=*) = lift2 (<=)

predicate (Behavior f) = (Event (\ x -> map g (f x)))
  where g True = Just ()
        g False = Nothing
        
ex12 = wander x 0 yellow
       where xvel = 1 `stepAccum` (hit ->> negate)
             x = integral xvel
             left = (x <=* (-2.0)) &&* (xvel <*0)
             right = x >=* 2.0  &&* xvel >*0 
             hit = predicate (left ||* right)
             
mouseDot = 
   mm =>> \ (x,y) -> 
            translate (constB x,constB y)            
                      dot

ex13 = paint red (dot `switch` mouseDot)
             



       
       
       
       