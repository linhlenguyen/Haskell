module ValuesCommands where

import Data.Time.Clock 
import Data.Time.LocalTime

-- In this worksheet replace you will discover the 
-- similarities and differences between Values and Commands.


-- A value is something you can compute without performing
-- any side effects to the world. Every time you compute it
-- you always get the same answer.

twentyNine :: Int
twentyNine = 20 + 9

backwards :: String
backwards = reverse "abcdefghijklmnopqrstuvwxyz"


-- A command is a computation that produces a value, but might
-- also perform a side effect that alters the state of the world.
-- If you perform a command, you might get a different resulting
-- value depending upon the state of the world.

printMyName :: IO ()
printMyName = putStrLn "Tim Sheard"

-- Commands can be sequenced, and combined using the "do"-notation

zone :: IO String
zone = do { t <- getCurrentTime
          ; z <- getTimeZone t
          ; return(show z)}


