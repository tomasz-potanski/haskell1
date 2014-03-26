module TreeDict
( Dict
, empty
, insert
, lookup
, fromList
, fromList2
, toList
, toListSwap
) where

import Prelude hiding(lookup)

data Dict a b = Leaf | Node a b Int (Dict a b) (Dict a b) deriving (Show, Eq)

-----------------------
empty :: Dict k v
empty = Leaf

------------------------
insert :: (Ord k) => k -> v -> Dict k v -> Dict k v
insert kk vv Leaf = (Node kk vv 1 Leaf Leaf)
insert kk vv (Node k v hh t u)
    | kk == k = (Node k vv hh t u)
    | kk < k && (balFactor ti u) ==  2 && kk < key t = 
			let aa = balanceLL(Node k v hh ti u) in changeH (aa) ((max (height (left aa)) (height (right aa)))+1)
    | kk < k && (balFactor ti u) ==  2 && kk > key t = 
			let aa = balanceLR (Node k v hh ti u) in changeH (aa) ((max (height (left aa)) (height (right aa)))+1)
    | kk > k && (balFactor t ui) == -2 && kk < key u = 
			let aa = balanceRL(Node k v hh t ui) in changeH (aa) ((max (height (left aa)) (height (right aa))) + 1)
    | kk > k && (balFactor t ui) == -2 && kk > key u = 
			let aa = balanceRR (Node k v hh t ui) in changeH (aa) ((max (height (left aa)) (height (right aa)))+1)
    | kk < k  = changeH (Node k v hh ti u) ((max (height ti) (height u)) + 1)
    | kk > k  = changeH (Node k v hh t ui) ((max (height t) (height ui)) + 1)
        where ti = insert kk vv t
              ui = insert kk vv u
			  
insert3 kk Leaf = (Node kk 1 1 Leaf Leaf)
insert3 kk (Node k v hh t u)
    | kk == k = (Node k (v+1) hh t u)
    | kk < k && (balFactor ti u) ==  2 && kk < key t = 
			let aa = balanceLL(Node k v hh ti u) in changeH (aa) ((max (height (left aa)) (height (right aa)))+1)
    | kk < k && (balFactor ti u) ==  2 && kk > key t = 
			let aa = balanceLR (Node k v hh ti u) in changeH (aa) ((max (height (left aa)) (height (right aa)))+1)
    | kk > k && (balFactor t ui) == -2 && kk < key u = 
			let aa = balanceRL(Node k v hh t ui) in changeH (aa) ((max (height (left aa)) (height (right aa))) + 1)
    | kk > k && (balFactor t ui) == -2 && kk > key u = 
			let aa = balanceRR (Node k v hh t ui) in changeH (aa) ((max (height (left aa)) (height (right aa)))+1)
    | kk < k  = changeH (Node k v hh ti u) ((max (height ti) (height u)) + 1)
    | kk > k  = changeH (Node k v hh t ui) ((max (height t) (height ui)) + 1)
        where ti = insert3 kk t
              ui = insert3 kk u
			  
insert2 (kk,vv) n = insert kk vv n
------------------------
lookup :: (Ord k) => k -> Dict k v -> Maybe v
lookup k Leaf = Nothing
lookup kk (Node k v h l r) 
		| (kk == k) = Just v
		| (kk < k) = lookup kk l
		| (kk > k) = lookup kk r
		| otherwise = Nothing
		
		
-------------------------
fromList :: Ord k => [(k, v)] -> Dict k v
fromList list = foldr (insert2) empty list

fromList2 list = foldr (insert3) empty list


-------------------------
toList :: Dict k v -> [(k, v)]
toList Leaf = []
toList (Node v v1 _ t u) = (toList t) ++ [(v,v1)] ++ (toList u)

toListSwap :: (Num v) => Dict k v -> [(v, k)]
toListSwap Leaf = []
toListSwap (Node v v1 _ t u) = (toListSwap t) ++ [((0-v1),v)] ++ (toListSwap u)

------------------------
depth :: Dict a b -> Int
depth Leaf = 0
depth (Node _ _ h _ _) = h

left (Node _ _ _ t _) = t
right (Node _ _ _ _ u) = u
value (Node _ v _ _ _) = v
key (Node k _ _ _ _) = k
height Leaf = 0
height (Node _ _ h _ _) = h

balFactor :: (Ord a) => Dict a b -> Dict a b -> Int
balFactor t u = (depth t) - (depth u)

changeH (Node k v hh t u) h = (Node k v h t u)

balanceLL (Node v vv h (Node vl vvl hl tl ul) u)              = (Node vl vvl h tl (Node v vv ((max (height ul) (height u)) + 1) ul u))
balanceLR (Node v vv h (Node vl vvl hl tl (Node vlr vvlr hlr tlr ulr)) u) = (Node vlr vvlr (hlr+2) (Node vl vvl ((max (height tl) (height tlr)) + 1) tl tlr) (Node v vv ((max (height ulr) (height u)) + 1) ulr u))
balanceRL (Node v vv h t (Node vr vvr hr (Node vrl vvrl hrl trl url) ur)) = (Node vrl vvrl (hrl+2) (Node v vv ((max (height t) (height trl)) + 1) t trl) (Node vr vvr ((max (height url) (height ur)) + 1) url ur)) 
balanceRR (Node v vv h t (Node vr vvr hr tr ur))              = (Node vr vvr h (Node v vv (h-1) t tr) ur)
 