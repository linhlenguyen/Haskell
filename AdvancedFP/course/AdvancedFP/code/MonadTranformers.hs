--  Code modifed from the paper "Monad Transformers Step by Step"
--  Author  Martin Grabmüller               
--  http://catamorph.de/publications/2004-10-01-monad-transformers.html

module Transformers where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import qualified Data.Map as Map


type Name   =  String                -- variable names

data Exp    =  Lit Integer           -- expressions
            |  Var Name
            |  Plus Exp Exp
            |  Abs Name Exp
            |  App Exp Exp
            deriving (Show)

data Value  =  IntVal Integer        -- values
            |  FunVal Env Name Exp
            deriving (Show)

type Env    =  Map.Map Name Value    -- mapping from names to values

-----------------------------------------------------
-- Simple, non monadic version

eval0                   ::  Env -> Exp -> Value
eval0 env (Lit i)       =   IntVal i
eval0 env (Var n)       =   fromJust (Map.lookup n env)
eval0 env (Plus e1 e2)  =   let  IntVal i1  = eval0 env e1
                                 IntVal i2  = eval0 env e2
                            in IntVal (i1 + i2)
eval0 env (Abs n e)     =   FunVal env n e
eval0 env (App e1 e2)   =   let  val1  = eval0 env e1
                                 val2  = eval0 env e2
                            in case val1 of
                                 FunVal env' n body -> eval0 (Map.insert n val2 env') body


e1 = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

a1 = eval0 Map.empty e1

-----------------------------------------------------------

type Eval1 alpha  =   Identity alpha

runEval1          ::  Eval1 alpha -> alpha
runEval1 ev       =   runIdentity ev


-- eval1 ::  Env -> Exp -> Eval1 Value
eval1 :: Monad m => Env -> Exp -> m Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = 
  maybe (fail ("undefined variable: " ++ n)) 
        return
        (Map.lookup n env)
--  recall that: maybe :: b -> (a -> b) -> Maybe a -> b
eval1 env (Plus e1 e2)  =   
  do  IntVal i1  <- eval1 env e1
      IntVal i2  <- eval1 env e2
      return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) =   
  do  val1  <- eval1 env e1
      val2  <- eval1 env e2
      case val1 of
         FunVal env' n body ->
           eval1 (Map.insert n val2 env') body
           
a2 = runEval1 (eval1 Map.empty e1)      

-------------------------------------------------------------
-- Adding exceptions, raising and reporting errors.

type Eval2 alpha = ExceptT String Identity alpha

runEval2     :: Eval2 alpha -> Either String alpha
runEval2 ev  = runIdentity (runExceptT ev)


eval2a ::  Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = 
  maybe (fail ("undefined variable: " ++ n)) 
        return
        (Map.lookup n env)
eval2a env (Plus e1 e2)  =   
  do  IntVal i1  <- eval2a env e1
      IntVal i2  <- eval2a env e2
      return $ IntVal (i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2)   =   
  do  val1  <- eval2a env e1
      val2  <- eval2a env e2
      case val1 of
         FunVal env' n body ->
            eval2a (Map.insert n val2 env') body
            
a3 = runEval2 (eval2a Map.empty e1)
a3x = runEval2 (eval2a Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))


-- Better error messages

eval2b ::  Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) =
  maybe (fail ("undefined variable: " ++ n)) 
        return
        (Map.lookup n env)
eval2b env (Plus e1 e2) =
   do  e1'  <- eval2b env e1
       e2'  <- eval2b env e2
       case (e1', e2') of
         (IntVal i1, IntVal i2) ->
             return $ IntVal (i1 + i2)
         _ -> throwError "type error"
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) =
   do  val1  <- eval2b env e1
       val2  <- eval2b env e2
       case val1 of
          FunVal env' n body ->
             eval2b (Map.insert n val2 env') body
          _ -> throwError "type error"
          
a3y = runEval2 (eval2b Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))          

-- Clear, concise and and more precise error messages. Exploits


eval2 ::  Env -> Exp -> Eval2 Value
eval2  env (Lit i) = return $ IntVal i
eval2  env (Var n) = 
  case Map.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> return val
eval2  env (Plus e1 e2) = 
   do  e1'  <- eval2  env e1
       e2'  <- eval2  env e2
       case (e1', e2') of
         (IntVal i1, IntVal i2) ->
             return $ IntVal (i1 + i2)
         _ -> throwError "type error in addition"
eval2  env (Abs n e) = return $ FunVal env n e
eval2  env (App e1 e2) = 
   do  val1  <- eval2  env e1
       val2  <- eval2  env e2
       case val1 of
          FunVal env' n body ->
             eval2  (Map.insert n val2 env') body
          _ -> throwError "type error in application"
-- 
a3z = runEval2 (eval2 Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))   



throwError1:: Monad m => String -> ExceptT String m a
throwError1 = throwError

catchError1 :: Monad m => ExceptT String m a -> 
                          (String -> ExceptT String m a) -> 
                          ExceptT String m a
catchError1 = catchError


{-  Note they also have a more general type

catchError :: MonadError e m => m a -> (e -> m a) -> m a
throwError :: MonadError e m => e -> m a

which comes from this class

class (Monad m) => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a
    
and this instance

Monad m => Monad (ExceptT e m)

-}

---------------------------------------------------------
-- Adding an environment (make the environment implicit)

type Eval3 alpha = ReaderT Env (ExceptT String Identity) alpha

runEval3 :: Env -> Eval3 alpha -> Either String alpha
runEval3 env ev = runIdentity (runExceptT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) =   
   do  env <- ask
       case Map.lookup n env of
          Nothing -> throwError ("unbound variable: " ++ n)
          Just val -> return val
eval3 (Plus e1 e2) =   
   do  e1'  <- eval3 e1
       e2'  <- eval3 e2
       case (e1', e2') of
         (IntVal i1, IntVal i2) ->
             return $ IntVal (i1 + i2)
         _ -> throwError "type error in addition"
eval3 (Abs n e) = 
   do  env <- ask
       return $ FunVal env n e
eval3 (App e1 e2) =   
   do  val1  <- eval3 e1
       val2  <- eval3 e2
       case val1 of
          FunVal env' n body ->
             local (const (Map.insert n val2 env'))
               (eval3 body)
          _ -> throwError "type error in application"
          
a4 = runEval3 Map.empty (eval3 e1)    
-- ask :: MonadReader r m => m r

------------------------------------------------------------
-- Adding State

type Eval4 alpha = ReaderT Env (ExceptT String (StateT Integer Identity)) alpha

runEval4 ::  Env -> Integer -> Eval4 alpha -> (Either String alpha, Integer)
runEval4 env st ev =
  runIdentity (runStateT (runExceptT (runReaderT ev env)) st)
  
  
tick :: (Num s, MonadState s m) => m ()
tick = do  st <- get
           put (st + 1)


eval4 :: Exp -> Eval4 Value
eval4 (Lit i) =
  do  tick
      return $ IntVal i
eval4 (Var n) = 
  do  tick
      env <- ask
      case Map.lookup n env of
         Nothing -> throwError ("unbound variable: " ++ n)
         Just val -> return val
eval4 (Plus e1 e2) = 
  do  tick
      e1'  <- eval4 e1
      e2'  <- eval4 e2
      case (e1', e2') of
        (IntVal i1, IntVal i2) ->
            return $ IntVal (i1 + i2)
        _ -> throwError "type error in addition"
eval4 (Abs n e)     =   
   do  tick
       env <- ask
       return $ FunVal env n e
eval4 (App e1 e2)   =
   do  tick
       val1  <- eval4 e1
       val2  <- eval4 e2
       case val1 of
          FunVal env' n body ->
             local (const (Map.insert n val2 env'))
               (eval4 body)
          _ -> throwError "type error in application"
          
a5 = runEval4 Map.empty 0 (eval4 e1)           


-- gets :: MonadState s m => (s -> a) -> m a
-- get :: MonadState s m => m s  
-- put :: MonadState s m => s -> m ()
------------------------------------------------
-- Switching the order of the monad transformers

type Eval4' alpha =   
   ReaderT Env 
   (StateT Integer 
   (ExceptT String Identity)) 
   alpha


runEval4' ::  Env -> Integer -> Eval4' alpha -> (Either String (alpha, Integer))
runEval4' env st ev  =   runIdentity (runExceptT (runStateT (runReaderT ev env) st))


-----------------------------------------------------------------------------
-- Writers for logging

type Eval5 alpha = 
   ReaderT Env  
   (ExceptT String 
   (WriterT [String] 
   (StateT Integer Identity))) alpha


runEval5 :: Env -> Integer -> Eval5 alpha -> ((Either String alpha, [String]), Integer)
runEval5 env st ev  =   
    runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)
    
    
    
eval5 ::  Exp -> Eval5 Value
eval5 (Lit i)       =   
  do  tick
      return $ IntVal i
eval5 (Var n)       =   
  do  tick
      tell [n]
      env <- ask
      case Map.lookup n env of
         Nothing -> throwError ("unbound variable: " ++ n)
         Just val -> return val
eval5 (Plus e1 e2)  =   
  do  tick
      e1'  <- eval5 e1
      e2'  <- eval5 e2
      case (e1', e2') of
        (IntVal i1, IntVal i2) ->
            return $ IntVal (i1 + i2)
        _ -> throwError "type error in addition"
eval5 (Abs n e)     =   
  do  tick
      env <- ask
      return $ FunVal env n e
eval5 (App e1 e2)   =  
  do  tick
      val1  <- eval5 e1
      val2  <- eval5 e2
      case val1 of
         FunVal env' n body ->
            local (const (Map.insert n val2 env'))
              (eval5 body)
         _ -> throwError "type error in application"    