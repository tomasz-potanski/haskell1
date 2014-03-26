module BST
( Tree
, empty
, depth
, toList
, fromList
, insert
) where

--I store depth of node
data Tree a = Leaf | Node a Int (Tree a) (Tree a) deriving (Show, Eq)
 
--------------------------
empty :: Tree a
empty = Leaf
 
-------------------------
depth :: Tree a -> Int
depth Leaf = 0
depth (Node _ h _ _) = h

----------------------------
toList :: Tree a -> [a]
toList Leaf = []
toList (Node v _ t u) = (toList t) ++ [v] ++ (toList u)

--------------------------------
fromList :: (Ord a) => [a] -> Tree a
fromList list = foldr (insert) empty list

 
--------------------------
insert :: (Ord a) => a -> Tree a -> Tree a
insert i Leaf = (Node i 1 Leaf Leaf)
insert i (Node v hh t u)
--    | i == v = (Node v t u)
    | i <= v && (balFactor ti u) ==  2 && i <= value t = 
			let aa = balanceLL(Node v hh ti u) in changeH (aa) ((max (height (left aa)) (height (right aa)))+1)
    | i <= v && (balFactor ti u) ==  2 && i > value t = 
			let aa = balanceLR (Node v hh ti u) in changeH (aa) ((max (height (left aa)) (height (right aa)))+1)
    | i > v && (balFactor t ui) == -2 && i <= value u = 
			let aa = balanceRL(Node v hh t ui) in changeH (aa) ((max (height (left aa)) (height (right aa))) + 1)
    | i > v && (balFactor t ui) == -2 && i > value u = 
			let aa = balanceRR (Node v hh t ui) in changeH (aa) ((max (height (left aa)) (height (right aa)))+1)
    | i <= v  = changeH (Node v hh ti u) ((max (height ti) (height u)) + 1)
    | i > v  = changeH (Node v hh t ui) ((max (height t) (height ui)) + 1)
        where ti = insert i t
              ui = insert i u
--			  hti = height (ti)
--			  hui = height (ui)
--			  ht = height (t)
--			  hu = height (u)


--------------

left (Node _ _ t _) = t
right (Node _ _ _ u) = u
value (Node v _ _ _) = v
height Leaf = 0
height (Node _ h _ _) = h

balFactor :: (Ord a) => Tree a -> Tree a -> Int
balFactor t u = (depth t) - (depth u)

-- http://en.wikipedia.org/wiki/Image:Tree_Rebalancing.gif
changeH (Node v hh t u) h = (Node v h t u)

balanceLL (Node v h (Node vl hl tl ul) u)              = (Node vl h tl (Node v ((max (height ul) (height u)) + 1) ul u))
balanceLR (Node v h (Node vl hl tl (Node vlr hlr tlr ulr)) u) = (Node vlr (hlr+2) (Node vl ((max (height tl) (height tlr)) + 1) tl tlr) (Node v ((max (height ulr) (height u)) + 1) ulr u))
balanceRL (Node v h t (Node vr hr (Node vrl hrl trl url) ur)) = (Node vrl (hrl+2) (Node v ((max (height t) (height trl)) + 1) t trl) (Node vr ((max (height url) (height ur)) + 1) url ur)) 
balanceRR (Node v h t (Node vr hr tr ur))              = (Node vr h (Node v (h-1) t tr) ur)
 