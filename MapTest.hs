module Main(
main
) where
  import Data.Map.Strict

  testMap :: Map String Int
  testMap = fromList [("One", 1), ("Two", 2), ("Three", 3), ("Four", 4), ("Five", 5)]

  data Tree a = Empty | Cons a [Tree a] deriving (Show)

  assignID :: Int -> Tree a -> Tree (Int, a)
  assignID _ Empty = Empty
  assignID index (Cons a (x:xs)) =

  main :: IO ()
  main = putStrLn "Hello World"
