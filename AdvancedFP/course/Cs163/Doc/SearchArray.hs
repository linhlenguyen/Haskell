module SearchArray where

import ArrCommands

search:: Eq a => a -> Array(a,b) -> IO (Maybe b)

search key arr = 
  do { (low,hi) <- boundsArr arr
     ; worker (low,hi) key arr
     }

worker (low,hi) key arr | low > hi = return Nothing
worker (low,hi) key arr = 
  do { (k,v) <- readArr arr low
     ; if key==k
          then return(Just v)
          else worker (low+1,hi) key arr
     }

x = [(5,"Tim"),(3,"Tom"),(6,"Mary"),(5,"Ann")]

test1 = do { arr <- newListArr (0,3) x
           ; printArr arr 
           ; ans <- search 6 arr
           ; print ans
           ; ans2 <- search 16 arr
           ; print ans2
           ; ans3 <- findArr arr 5
           ; print ans3
           }
           
           
-- Return the first (lowest) index where "pred" is true
searchArr :: (a -> Bool) -> Array a -> IO Int
searchArr pred arr = undefined




-- Return the list of all the satelite data which
-- appears in the Array paired with the given key
findArr :: Eq a => Array (a,b) -> a -> IO [b]
findArr arr key = 
  do { (low,hi) <- boundsArr arr
     ; collect (low,hi) key arr
     }
     
collect :: Eq key => (Int,Int) -> key -> Array (key,v) -> IO [v]
collect (low,hi) key arr | low > hi = return []
collect (low,hi) key arr = 
  do { (k,v) <- readArr arr low
     ; stuff <- collect (low+1,hi) key arr
     ; return(if key==k
                 then (v : stuff)
                 else stuff)
     }



