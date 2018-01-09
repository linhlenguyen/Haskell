import System.Random

data Hex = ONE | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | ZERO | A | B | C | D | E | F deriving (Enum, Bounded)

instance Random Hex where
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

instance Show Hex where
    show ONE = "1"
    show TWO = "2"
    show THREE = "3"
    show FOUR = "4"
    show FIVE = "5"
    show SIX = "6"
    show SEVEN = "7"
    show EIGHT = "8"
    show NINE = "9"
    show ZERO = "0"
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"
    show E = "E"
    show F = "F"

main = do 
    g <- newStdGen
    print  $ concatMap (show) $ take 64 $ (randoms g :: [Hex])