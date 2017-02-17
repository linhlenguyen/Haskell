module Reader(

)
  where
    import Data.Map
    import Control.Monad.Reader

    type Context = Map Int String

    first :: Reader Context String
    first = do
      name <- ask
      return (name!1)

    second :: Reader Context String
    second = do
      anotherName <- ask
      return (anotherName!2)

    main :: IO ()
    main = putStrLn $ runReader startReading (Data.Map.fromList [(1,"Hello"), (2, "World")])
      where startReading :: Reader Context String
            startReading = do
              a <- first
              b <- second
              return (a ++ " " ++ b)
