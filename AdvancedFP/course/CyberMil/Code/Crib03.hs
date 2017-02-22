module Crib03 where

import Char

x1 = x + length y
  where x = 2 * 5
        y = [1
            ,2
            ,3
            ]
            


capitalize x = 
  let front = head x
      rear = tail x
  in toUpper front : rear  
  

  
main = do putStrLn greeting
          name <- getLine
          putStrLn 
            ("Hello, "++name++".")
   where
     greeting = "Type your name"
     
z = 0     