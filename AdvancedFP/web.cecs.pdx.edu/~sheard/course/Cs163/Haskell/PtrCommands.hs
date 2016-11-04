module PtrCommands(Ptr,newPtr,readPtr,writePtr,samePtr) where

import Data.IORef(newIORef,readIORef,writeIORef,IORef)

type Ptr = IORef

newPtr :: a -> IO (Ptr a)
readPtr :: Ptr a -> IO a
writePtr:: Ptr a -> a -> IO ()
samePtr:: Ptr a -> Ptr a -> Bool

newPtr = newIORef
readPtr = readIORef
writePtr = writeIORef
samePtr x y = x==y