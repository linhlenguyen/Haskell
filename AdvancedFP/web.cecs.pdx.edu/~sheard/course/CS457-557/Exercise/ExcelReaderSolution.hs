module ExcelReader where

import CSVReader(int,string,bool,list,row,blank,x,skip,many)
-- import CSVReader2     -- the version with line numbers
import CSV(parseCSVFromFile)
import Excell(col,rows,cols,stack,above,beside,blankRow)
import Control.Monad.State(evalState,execState)

tests1 = ["Tom","34","True","3","6","7"]

ex1 = evalState (string `x` int `x` string) tests1

-- (1) What type is ex1? Why isn't it (String,Int,String)?
--     What does ex1 look like (run it and see)

-- (2) Replace the undefined in ex2 so that it 
--     Reads the following ("Tom",(34,(True,[3,6,7])))

ex2:: (String,(Int,(Bool,[Int])))
ex2 = evalState (string `x` int `x` bool `x` list 3 int) tests1

-- (3) Write the lift function so that ex4 has the correct type and
--     Reads the following result

ex3:: (String,Int,Bool,[Int])
ex3 = lift ex2
  where lift (x,(y,(z,w))) = (x,y,z,w)

-- (4) Use the "row" operator to Read a list of 3 rows
--     to get the result
--     (["Tom","34","True","3","6","7"]
--     ,(("Tom",34,True,[3,6,7])
--     ,[34,3,6,7])
--     )

tests2 = [tests1,tests1,tests1]

ex4:: ([String],((String,Int,Bool,[Int]),[Int]))
ex4 = evalState ((row f (list 6 string)) `x` 
                 (row g (string `x` int `x` bool `x` list 3 int)) `x` 
                 (row h (skip `x` int `x` skip `x` list 3 int)) 
                ) tests2
  where f x = x
        g (x,(y,(z,w))) = (x,y,z,w)
        h (x,(y,(z,w))) = y:w



-- The file DemoMajors.csv is a comma separated values file that encodes
-- the Excell table listed at the very bottom of file.


-- (5) Use the operators from the CSVReader library to import the file
-- into Haskell as (String,String,[(String,Int,Int)]) 
-- your answer should look something like
-- ("Male","Female",[("Business",97,90),("Biology",36,41), ...]

reader = (row snd (skip `x` string `x` string)) `x`
         (many 47 (row triple (string `x` int `x` int)))
triple (x,(y,z)) = (x,y,z)        


getfile :: IO (String, String, [(String, Int, Int)])
getfile = 
  do { Right listOflistOfString <- parseCSVFromFile "DemoMajors.csv"
     ; let fix ((s1,s2),zs) = (s1,s2,zs)
     ; return (fix (evalState reader listOflistOfString))
     }

-- (6) Use the operators of the Excell library to recreate the table below
--     from the information you created in part (4) above.

main =
  do { (lab1,lab2,rs) <- getfile
     ; let table = (blankRow 1 `beside` rows [[lab1,lab2]]) `above` (col rs)
     ; putStrLn(show table)
  
     }


{- 



48 rows by 3 columns

+--------+----+------+
|        |Male|Female|
+--------+----+------+
|Business|97  |90    |
+--------+----+------+
|Biology |36  |41    |
+--------+----+------+
|Psycholo|12  |50    |
+--------+----+------+
|Art     |20  |39    |
+--------+----+------+
|English |17  |28    |
+--------+----+------+
|Internat|13  |32    |
+--------+----+------+
|Health S|16  |23    |
+--------+----+------+
|Music   |20  |14    |
+--------+----+------+
|Accounti|23  |10    |
+--------+----+------+
|Foreign |12  |21    |
+--------+----+------+
|Architec|20  |11    |
+--------+----+------+
|Computer|25  |1     |
+--------+----+------+
|Mechanic|21  |4     |
+--------+----+------+
|Politica|16  |9     |
+--------+----+------+
|Chemistr|9   |15    |
+--------+----+------+
|Civil En|20  |2     |
+--------+----+------+
|History |12  |9     |
+--------+----+------+
|Administ|9   |10    |
+--------+----+------+
|Communic|5   |13    |
+--------+----+------+
|Electric|15  |3     |
+--------+----+------+
|Film    |9   |9     |
+--------+----+------+
|Philosop|10  |5     |
+--------+----+------+
|Computer|13  |0     |
+--------+----+------+
|Theater |9   |4     |
+--------+----+------+
|Anthropo|4   |8     |
+--------+----+------+
|Child an|1   |11    |
+--------+----+------+
|Elementa|3   |9     |
+--------+----+------+
|Economic|5   |6     |
+--------+----+------+
|Environm|7   |4     |
+--------+----+------+
|Environm|2   |9     |
+--------+----+------+
|Science |5   |5     |
+--------+----+------+
|Engineer|7   |0     |
+--------+----+------+
|Physics |4   |3     |
+--------+----+------+
|Social S|3   |4     |
+--------+----+------+
|Social W|1   |6     |
+--------+----+------+
|Applied |3   |3     |
+--------+----+------+
|Mathemat|4   |1     |
+--------+----+------+
|Sociolog|0   |4     |
+--------+----+------+
|Geology |2   |1     |
+--------+----+------+
|Liberal |0   |2     |
+--------+----+------+
|Secondar|1   |1     |
+--------+----+------+
|Women's |0   |2     |
+--------+----+------+
|Black St|0   |1     |
+--------+----+------+
|Canadian|1   |0     |
+--------+----+------+
|East Asi|0   |1     |
+--------+----+------+
|European|0   |1     |
+--------+----+------+
|Middle E|0   |1     |
+--------+----+------+
-}