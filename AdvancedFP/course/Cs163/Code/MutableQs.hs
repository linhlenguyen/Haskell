module MutableQs where

import PtrCommands
import MutList

data Queue a = Q (Maybe (Ptr (MutList a),Ptr (MutList a)))

-- A Q has one of the following two structures
-- (Q Nothing)              -- the empty Q
-- (Q (Just (front,rear)))  -- A queue where front and rear are pointers

emptyQ :: Queue t -> Bool
emptyQ (Q Nothing) = True
emptyQ (Q (Just(front,rear))) = False

newQ = Q Nothing

enQueue :: a -> Queue a -> IO (Queue a)
enQueue elem (Q Nothing) = 
  do { list <- toMutList [elem]
     ; front <- newPtr list
     ; rear <- newPtr list
     ; return (Q (Just (front,rear))) }
enQueue elem (Q (Just(front,rear))) =
  do { (Cons elemptr listptr) <- readPtr rear
     ; list <- toMutList [elem]
     ; writePtr listptr list
     ; writePtr rear list 
     ; return (Q (Just(front,rear))) } 


deQueue:: Queue a -> IO(a,Queue a)     
deQueue (Q Nothing) = error "Noting to deQueue"
deQueue (Q (Just(front,rear))) =
  do { (Cons elemPtr listPtr) <- readPtr front
     ; elem <- readPtr elemPtr
     ; tl <- readPtr listPtr
     ; case tl  of
         Nil        -> return(elem,Q Nothing)
         (Cons _ _) -> return(elem,Q (Just(listPtr,rear)))
     }
     
main = 
  do { q1 <- enQueue (5::Int) newQ
     ; q2 <- enQueue 7 q1
     ; (n,q3) <- deQueue q2
     ; (m,q4) <- deQueue q3
     ; print (n,m,emptyQ q4)
     }