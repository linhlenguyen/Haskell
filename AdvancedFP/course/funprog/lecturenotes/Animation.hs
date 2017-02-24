-- <PRE>
module Animation where
import Shape
import Draw
import Region
import Picture
import SOEGraphics hiding (Region)
import qualified SOEGraphics as G (Region)
import Win32Misc (timeGetTime)
import Word (Word32,word32ToInt)
  
-------------- The animation stuff ----------

type Animation a = Time -> a
type Time = Float
 
animate :: String -> Animation a -> (a -> IO Graphic) -> IO ()
animate title anim toGraphic
  = runGraphics (
    do w <- openWindowEx title (Just (0,0)) (Just (xWin,yWin))
              drawBufferedGraphic (Just 30)
       t0 <- timeGetTime
       let loop =
             do t <- timeGetTime
                -- print ("the time is" ++ (show t))
                let ft = intToFloat (word32ToInt (t-t0)) / 1000
                -- print "past let"
                gr <- toGraphic (anim ft)
                -- print "past toGraphic"
                setGraphic w gr
                getWindowTick w
                loop
       loop
    )
 
regionToGraphic :: Region -> Graphic
regionToGraphic = drawRegion . regionToGRegion
   
 
picToGraphic :: Picture -> Graphic
picToGraphic (Region c r)
  = withColor c (regionToGraphic r)
picToGraphic (p1 `Over` p2)
  = picToGraphic p1 `overGraphic` picToGraphic p2
picToGraphic (Text v str) = (text (trans v) str)
picToGraphic EmptyPic = emptyGraphic

{-  Old Version
picToGraphic :: Picture -> IO Graphic
picToGraphic (Region c r)
  = do -- print "in Region before"
       gr <- regionToGraphic r
       -- print "in Region after regionToGraphic"
       return (withColor c gr)
picToGraphic (p1 `Over` p2)
  = do -- print "In over before"
       gr1 <- picToGraphic p1
       -- print "In Over after p1"
       gr2 <- picToGraphic p2
       -- print "In over after p2"
       return (gr1 `overGraphic` gr2)
picToGraphic (Text v str) = return (text (trans v) str)
picToGraphic EmptyPic
  = return emptyGraphic
-}


type Anim = Animation Picture

emptyA :: Anim
emptyA t = EmptyPic
overA :: Anim -> Anim -> Anim
overA a1 a2 t = a1 t `Over` a2 t

overManyA :: [Anim] -> Anim
overManyA = foldr overA emptyA

-- timeTransA :: (Time -> Time) -> Animation a -> Animation a
timeTransA :: Animation Time -> Animation a -> Animation a

-- timeTransA f a t = a (f t)
timeTransA f a = a . f
 
------- Type Classes and Animations ---------

newtype Behavior a = Beh (Time -> a)

lift0 :: a -> Behavior a
lift0 x = Beh (\t -> x)

lift1 :: (a -> b) -> (Behavior a -> Behavior b)
lift1 f (Beh a)
  = Beh (\t -> f (a t))

lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
lift2 g (Beh a) (Beh b)
  = Beh (\t -> g (a t) (b t))

lift3 :: (a -> b -> c -> d) ->
          (Behavior a -> Behavior b -> Behavior c -> Behavior d)
lift3 g (Beh a) (Beh b) (Beh c)
   = Beh (\t -> g (a t) (b t) (c t))

instance Eq (Behavior a) where
  a1 == a2 = error "Can't compare animations."

instance Show (Behavior a)  where
   showsPrec n a1 = error "Can't coerce animation to String."

instance Num a => Num (Behavior a) where
  (+) = lift2 (+);  (*) = lift2 (*)
  negate = lift1 negate; abs = lift1 abs
  signum = lift1 signum
  fromInteger = lift0 . fromInteger

instance Fractional a => Fractional (Behavior a) where
  (/) = lift2 (/)
  fromRational = lift0 . fromRational

instance Floating a => Floating (Behavior a) where
   pi    = lift0 pi;    sqrt = lift1 sqrt
   exp   = lift1 exp;   log = lift1 log
   sin   = lift1 sin;   cos = lift1 cos
   tan   = lift1 tan
   asin  = lift1 asin;  acos = lift1 acos
   atan  = lift1 atan
   sinh  = lift1 sinh;  cosh = lift1 cosh
   tanh  = lift1 tanh
   asinh = lift1 asinh; acosh = lift1 acosh
   atanh = lift1 atanh

time :: Behavior Time
time = Beh (\t -> t)

instance Ani [a] where
  empty = []
  over  = (++)

instance Ani (Fun a) where
  empty = Fun id
  Fun a `over` Fun b = Fun (a . b)

data Fun a = Fun (a->a)

class Ani a where
  empty :: a
  over  :: a -> a -> a

instance Ani Picture where
  empty = EmptyPic
  over  = Over

instance Ani a => Ani (Behavior a) where
  empty = lift0 empty
  over  = lift2 over

overMany :: Ani a => [a] -> a
overMany = foldr over empty

 
-- </PRE>
