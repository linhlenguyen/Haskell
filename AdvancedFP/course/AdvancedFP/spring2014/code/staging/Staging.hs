{-# LANGUAGE TemplateHaskell, DataKinds, TypeFamilies,
    GADTs #-}

module S where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax(lift)
import Language.Haskell.TH.PprLib

import UTI hiding (sh)


data Nat = Zero | Succ Nat

-- Type-level nat literals (almost).
--
-- Write '$(nat n)' for a literal 'n'.
nat :: Int -> Q Exp
nat 0 = [e| Zero |]
nat n = [e| Succ $(nat (n-1)) |]

sh:: Ppr a => Q a -> IO ()
sh x = 
  do str <- runQ(do { a <- x
                    ; return(show(ppr a))})
     putStrLn str



sumf 0 x = x
sumf n x = [e| \ y -> $(sumf (n-1) [e| $x + y |]) |]

pow :: Int -> Q Exp -> Q Exp
pow 0 x = [| 1 |]
pow 1 x = x
pow n x = [e| $x * $(pow (n-1) x) |]

power n = [e| \ x -> $(pow n [e| x |]) |]

test1 = $(splice1)
test2 = $(splice2)
-- test3 = $(splice3)
-- $(splice4)

 