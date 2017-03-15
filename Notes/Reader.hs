module Reader(

)
  where
    import Data.Map
    import Control.Monad.Reader

    type Context = Map Int String

    first :: Reader Context String
    first = do
      name <- ask
      local (\m -> insert 3 "!" m) $ return (name!1)

    second :: Reader Context String
    second = do
      anotherName <- ask
      return ((anotherName!2) ++ (anotherName!3))

    main :: IO ()
    main = putStrLn $ runReader startReading (Data.Map.fromList [(1,"Hello"), (2, "World")])
      where startReading :: Reader Context String
            startReading = do
              a <- first
              -- m a -> (a -> m b) -> m b
              -- 
              b <- second
              return (a ++ " " ++ b)

-- Reader env a = Reader { runReader :: env -> a}
--
-- m a -> (a -> m b) -> m b
-- (Reader env) a -> (a -> (Reader env) b) -> (Reader env) b
--
-- instance Monad (Reader env) where
-- return x = Reader (\_ -> x)
-- f >>= g = Reader (\env -> runReader (g (runReader f env)) env)
