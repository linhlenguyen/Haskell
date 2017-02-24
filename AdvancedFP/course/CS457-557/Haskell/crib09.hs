module Crib09 where
-- IORef and assignment is one way of creating
-- and usimg mutable variables in Haskell. They
-- are restricted to use in the IO monad.


-- These imports are necessary to use refereces 
-- and assignment in the IO monad.
import Data.IORef(newIORef,readIORef,writeIORef,IORef)
import System.IO.Unsafe(unsafePerformIO)

-- Using IORefs as global flags or counters

-- creating a Global reference
count = unsafePerformIO (newIORef 0)





-- an operation that access the reference
next = 
  do { n <- readIORef count
     ; writeIORef count (n+1)
     ; return (n+1) }
     



-- Using IORefs locally
fact n = 
  do { ans <- newIORef 1
     ; let loop i = 
             do { m <- readIORef ans
                ; writeIORef ans (m * i)
                ; if i >= n
                     then return ()
                     else loop (i+1) }
     ; loop 1
     ; readIORef ans }