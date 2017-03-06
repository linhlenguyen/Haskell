-- Enter your code here. Read input from STDIN. Print output to STDOUT
rows = 32
columns = 63

heights = [32, 16, 8, 4, 2, 1]

firstTip = 32::Int

--[[t1]] -> [[t1] [t2 t3]] -> [[t1] [t4 t5] [t2 t3] [t6 t7 t8 t9]]
--[t1] [t10 t11] [t4 t5] [t12 t13 t14 t15] [t2 t3] [t6 t7 t8 t9]

newTip :: Int -> Int -> [[Int]]
newTip height x = [x - height, x + height]

triangleExpansion :: [[Int]]
triangleExpansion = [[firstTip]] : beginExpansion [[firstTip]] 1
    where beginExpansion :: [[Int]] -> Int -> [[Int]]
          beginExpansion ls n = let newLs = map (\x -> (map (newTip (heights!!n)) x)) ls in newLs : beginExpansion newLs (n+1)
