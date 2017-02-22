
-- A, B, C, D, and E are types, that will appear
-- as indexes in other types.

data Tag':: *1 where
  A:: Tag'
  B:: Tag'
  C:: Tag'
  D:: Tag'

-- A Shape is a list or record structure in which
--  A, B, C, D, and E are the labels.

data Shape:: *1 where
  E :: Shape
  P :: Tag' ~> *0 ~> Shape ~> Shape
    deriving Record(s)  
  
  
-- A label is a value (i.e. data manipulated by a program)
-- It is also a singleton type, that reflects the types
--  A, B, C, D, and E.

data Label' :: Tag' ~> *0  where
  A:: Label' A
  B:: Label' B
  C:: Label' C
  D:: Label' D

-- A count is a natural number, which is indexed
-- by shapes and types.

data Count:: Shape ~> Tag' ~> *0 ~> *0 where
  Zero:: Count (P a t b) a t
  Succ:: Count sh nm ty -> (Count (P a ty2 sh)) nm ty
    deriving Nat(c)

-- A Term is indexed by a Shape which determines what
-- variables are used in the term, and what type those
-- variables have.

data Term:: Shape ~> *0 ~> *0 where
  Iconst :: Int -> Term sh Int
  Var:: Label' a -> Count i a ty -> Term i ty
  Pair:: Term sh a -> Term sh b -> Term sh (a,b)
  
  
  

  
  