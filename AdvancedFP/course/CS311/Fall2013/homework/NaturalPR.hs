module PR
where
import Prelude hiding (pred,and,or,not,sqrt,div)
import Data.Maybe (mapMaybe,isJust)
import Control.Monad (guard)
import Data.List (inits)

data PR =  Z
         | S	
         | P Int
         | C PR [PR]
         | PR PR PR
         deriving (Show, Eq)

eval :: PR -> [Integer] -> Integer
eval Z _ = 0
eval S [x] = x+1
eval S _  = 0 -- relaxed
eval (P n) xs = nth n xs
eval (C f gs) xs = eval f (map (\g -> eval g xs) gs)
eval (PR g h) (0:xs) = eval g xs
eval (PR g h) (x:xs) = eval h ((x-1) : eval (PR g h) ((x-1):xs) : xs)
eval (PR _ _) [] = 0  -- relaxed

nth _ [] = 0 -- error "nth nil"
nth 0 _ = 0 -- error "nth index"
nth 1 (x:_) = x
nth (n) (_:xs) = nth (n-1) xs

check :: PR -> Int -> Bool
check Z _ = True
check S 1 = True
check (P n) m = n <= m
check (C f gs) n = check f (length gs) && (all (\g -> check g n) gs)
check (PR g h) n = check g (n-1) && check h (n+1)

-- to test a function 'f' with n arguments
-- type:  eval f [x1, ..., xn]
-- e.g.    eval plus [3,5]   
-- will return 8