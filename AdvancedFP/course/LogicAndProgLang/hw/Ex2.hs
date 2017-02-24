module Ex2 where

import SimpleProp


simplify:: Prop n -> Prop n
simplify x = undefined

-- it s OK to add constraints to simplify
-- e.g.   simplify:: Eq n => Prop n -> Prop n