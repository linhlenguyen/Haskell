import System.Environment

main' :: IO ()
main' = getArgs >>= (\[arg1,arg2] -> putStrLn arg1 >> putStrLn arg2)

getInput :: Char
getInput = getChar

main :: IO ()
main = 
