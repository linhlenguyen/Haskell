import Control.Monad.Reader

data Term = Apply Term Term | Lambda String Term | Var String deriving (Show)

newtype Env = Env ([(String,Closure)]) deriving (Show)
type Closure = (Term,Env)

data Value = Lam String Closure | Failure String deriving (Show)

{--
instance Show Value where
  show (Failure _) = "Failure"
  show (Lam s c) =
--}

interp' :: Term -> Reader Env Value
interp' (Lambda nv t)
   = do env <- ask
        return $ Lam nv (t,env)
interp' (Var v)
   = do (Env env) <- ask
        case lookup (show v) env of
          Nothing -> return . Failure $ "unbound variable: " ++ (show v)
          Just (term,env) -> local (const env) $ interp' term

interp' (Apply t1 t2)
   = do v1 <- interp' t1
        case v1 of
           Failure s -> return (Failure s)
           Lam nv clos -> local (\(Env ls) -> Env ((nv,clos):ls)) $ interp' t2

interp :: Term -> Value
interp term = runReader (interp' term) (Env [])

sample :: Term
sample = (Apply (Lambda "x" (Var "x + 1")) (Var "1"))
