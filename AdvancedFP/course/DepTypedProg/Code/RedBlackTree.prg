----------------------------------------
-- Introduce a new kind to represent colors

kind Color  = Red | Black

-----------------------------------------
-- Top-level type that hides both 
-- color of the node and tree height

data RBTree:: *0 where
 Root:: SubTree Black n -> RBTree

----------------------------------------
-- GADT that captures invariants

data SubTree:: Color ~> Nat ~> *0 where
 Leaf:: SubTree Black Z
 RNode:: SubTree Black n ->
         Int ->
         SubTree Black n ->
         SubTree Red n
 BNode:: SubTree cL m ->
         Int ->
         SubTree cR m ->
         SubTree Black (S m)

---------------------------------------------------
-- A Ctxt records where we've been as we descend 
-- down into a tree as we search for a value

data Dir = LeftD | RightD

data Ctxt:: Color ~> Nat ~> *0 where
  Nil:: Ctxt Black n
  RCons:: Int -> Dir ->
          SubTree Black n ->
          Ctxt Red n ->
          Ctxt Black n
  BCons:: Int -> Dir ->
          SubTree c1 n ->
          Ctxt Black (S n) ->
          Ctxt c n

-------------------------------------------
-- Turn a Red tree into a black tree. Always
-- possible, since Black nodes do not restrict
-- the color of their sub-trees.

blacken :: SubTree Red n -> SubTree Black (S n)
blacken (RNode l e r) = (BNode l e r)


------------------------------------------
-- A singleton type representing Color at
-- the value level.

data CRep :: Color ~> *0 where
  Red   :: CRep Red
  Black :: CRep Black

color :: SubTree c n -> CRep c
color Leaf = Black
color (RNode _ _ _) = Red
color (BNode _ _ _) = Black

-------------------------------------------------------
-- fill a context with a subtree to regain the original 
-- RBTree, works if the colors and black depth match up

fill :: Ctxt c n -> SubTree c n -> RBTree
fill Nil t = Root t
fill (RCons e LeftD  uncle c) tree = fill c (RNode uncle e tree)
fill (RCons e RightD uncle c) tree = fill c (RNode tree  e uncle)
fill (BCons e LeftD  uncle c) tree = fill c (BNode uncle e tree)
fill (BCons e RightD uncle c) tree = fill c (BNode tree  e uncle)

insert :: Int -> RBTree -> RBTree
insert e (Root t) = insert_ e t Nil

-------------------------------------------------------------
-- as we walk down the tree, keep track of everywhere 
-- we've been in the Ctxt input.

insert_ :: Int -> SubTree c n -> Ctxt c n -> RBTree
insert_ e (RNode l e' r) ctxt
        | e < e'        = insert_ e l (RCons e' RightD r ctxt)
        | True          = insert_ e r (RCons e' LeftD  l ctxt)
insert_ e (BNode l e' r) ctxt
        | e < e'        = insert_ e l (BCons e' RightD r ctxt)
        | True          = insert_ e r (BCons e' LeftD  l ctxt)
-- once we get to the bottom we "insert" the node as a Red node.
-- since this may break invariant, we may need do some patch work
insert_ e Leaf ctxt = repair (RNode Leaf e Leaf) ctxt

-------------------------------------------------------
-- Repair a tree if its out of balance. The Ctxt holds
-- crucial information about colors of parent and 
-- grand-parent nodes.

repair :: SubTree Red n -> Ctxt c n -> RBTree
repair t (Nil)                 = Root (blacken t)
repair t (BCons e LeftD  sib c) = fill c (BNode sib e t)
repair t (BCons e RightD sib c) = fill c (BNode t e sib)
-- these are the tricky cases
repair t (RCons e dir sib (BCons e' dir' uncle ctxt)) =
  case color uncle of
    Red   -> repair (recolor dir e sib dir' e' (blacken uncle) t) ctxt
    Black -> fill ctxt (rotate dir e sib dir' e' uncle t)
repair t (RCons e dir sib (RCons e' dir' uncle ctxt)) = unreachable

recolor :: Dir -> Int -> SubTree Black n ->
           Dir -> Int -> SubTree Black (S n) ->
           SubTree Red n -> SubTree Red (S n)
recolor LeftD  pE sib RightD gE uncle t = RNode (BNode sib pE t) gE uncle
recolor RightD pE sib RightD gE uncle t = RNode (BNode t pE sib) gE uncle
recolor LeftD  pE sib LeftD  gE uncle t = RNode uncle gE (BNode sib pE t)
recolor RightD pE sib LeftD  gE uncle t = RNode uncle gE (BNode t pE sib)

rotate :: Dir -> Int -> SubTree Black n ->
          Dir -> Int -> SubTree Black n ->
          SubTree Red n -> SubTree Black (S n)
rotate RightD pE sib RightD gE uncle (RNode x e y) = BNode (RNode x e y) pE (RNode sib gE uncle)
rotate LeftD  pE sib RightD gE uncle (RNode x e y) = BNode (RNode sib pE x) e (RNode y gE uncle)
rotate LeftD  pE sib LeftD  gE uncle (RNode x e y) = BNode (RNode uncle gE sib) pE (RNode x e y)
rotate RightD pE sib LeftD  gE uncle (RNode x e y) = BNode (RNode uncle gE x) e (RNode y pE sib)

delete :: Int -> RBTree -> RBTree
delete e (Root t) = delete_ e t Nil

-----------------------------------------------------------------
-- as we walk down the tree, keep track of everywhere we've been

delete_ :: Int -> SubTree c n -> Ctxt c n -> RBTree
delete_ e (RNode l e' r) ctxt
        | e == e'       = replaceRed l r ctxt
        | e < e'        = delete_ e l (RCons e' RightD r ctxt)
        | True          = delete_ e r (RCons e' LeftD  l ctxt)
delete_ e (BNode l e' r) ctxt
        | e == e'       = replaceBlack l r ctxt
        | e < e'        = delete_ e l (BCons e' RightD r ctxt)
        | True          = delete_ e r (BCons e' LeftD  l ctxt)
-- e isn't in the tree
delete_ e Leaf ctxt = fill ctxt Leaf

replaceRed :: SubTree Black n -> SubTree Black n -> Ctxt Red n -> RBTree
replaceRed l    Leaf (BCons e RightD sib ctxt) = fill ctxt (BNode l e sib)
replaceRed l    Leaf (BCons e LeftD  sib ctxt) = fill ctxt (BNode sib e l)
replaceRed Leaf r    (BCons e RightD sib ctxt) = fill ctxt (BNode r e sib)
replaceRed Leaf r    (BCons e LeftD  sib ctxt) = fill ctxt (BNode sib e r)
replaceRed l (r@(BNode _ _ _)) ctxt           = undefined

replaceBlack :: SubTree cL n -> SubTree cR n -> Ctxt Black (S n) -> RBTree
replaceBlack Leaf Leaf ctxt = undefined -- do something clever here

spliceOutMin :: SubTree c (S n) -> Ctxt c (S n) -> (RBTree,e)
spliceOutMin (BNode Leaf e r) ctxt = undefined -- (fill ctxt undefined,e)
