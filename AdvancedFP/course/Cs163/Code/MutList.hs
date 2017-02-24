module MutList where

import PtrCommands

data MutList a = Nil | Cons (Ptr a) (Ptr (MutList a))

toMutList :: [a] -> IO(MutList a)
toMutList [] = return Nil
toMutList (x:xs) =
  do { xp <- newPtr x
     ; xsp <- toMutList xs
     ; tailp <- newPtr xsp
     ; return(Cons xp tailp)
     }
     
fromMutList :: MutList a -> IO[a]
fromMutList Nil = return[]
fromMutList (Cons xp xsp) = 
  do { x <- readPtr xp
     ; xs' <- readPtr xsp
     ; xs <- fromMutList xs'
     ; return(x:xs) }
     

showMutList :: Show a => MutList a -> IO()
showMutList xs =
  do { ys <- fromMutList xs
     ; print ys }

change _ Nil x = return ()     
change 0 (Cons p xs) x = writePtr p x
change n (Cons p xsp) x = 
  do { xs <- readPtr xsp
     ; change (n-1) xs x }
     
destrApp Nil ys = return()
destrApp (Cons x xs) ys =
  do { ws <- readPtr xs
     ; case ws of
        Nil -> writePtr xs ys
        (Cons _ _) -> destrApp ws ys }