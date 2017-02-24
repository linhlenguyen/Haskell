module DoublyLinked where

import PtrCommands

data DLL a = Nil  | Cons3 a (Ptr (DLL a)) (Ptr (DLL a))

addToFront :: a -> DLL a -> IO (DLL a)
addToFront a Nil = undefined
addToFront a (Cons b forward backward) = undefined