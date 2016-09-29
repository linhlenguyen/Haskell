hello = putStr "Hello " >> putStr "World! " >> putStr "I come in peace. " >> getLine >>= (\x -> putStrLn x)

main = do putStr "What is your first name ?"
          firstName <- getLine
          putStr "And your last name ?"
          lastName <- getLine
          let full = firstName ++ " " ++ lastName
          putStrLn ("Please to you meet you, " ++ full ++ " !")

--IO Monad
--Do statement and Monad
--putStr string >> getLine >>= (\firstName -> )
