import System.Environment

main :: IO ()
main = getArgs >>= (\[arg1,arg2] -> putStrLn arg1 >> putStrLn arg2)
