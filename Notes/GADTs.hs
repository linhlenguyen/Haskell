

--ADTs
data Expr = I Int
			| Add Expr Expr
			| Mul Expr Expr

eval :: Expr -> Int
eval (I n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

data Expr' = I Int
			| B Bool
			| Add Expr' Expr'
			| Mul Expr' Expr'
			| Eq Expr' Epxr'

eval' :: Expr -> Maybe (Either Int Bool)
eval' (I n) = Just $ Left n
eval' (B b) = Just $ Right b
eval' (Add Nothing _) = Nothing
eval' (Add _ Nothing) = Nothing
eval' (Add (B b) _) = Nothing
eval' (Add _ (B b) = Nothing
eval' (Add e1 e2) = eval e1 + eval e2 --Have to write + and * extention on Maybe
eval' (Mul Nothing _) = Nothing
eval' (Mul _ Nothing) = Nothing
eval' (Mul (B b) _) = Nothing
eval' (Mul _ (B b) = Nothing
eval' (Mul e1 e2) = eval e1 * eval e2 --Have to write + and * extention on Maybe

--GADTs

data Expr'' where
	I :: Int -> Expr Int
	B :: Bool -> Expr Bool
	Add :: Expr Int -> Expr Int -> Expr Int
	Mul :: Expr Int -> Expr Int -> Expr Int
	Eq :: Expr Int -> Expr Int -> Expr Bool

eval'' :: Expr a -> a
