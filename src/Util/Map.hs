{-# LANGUAGE NoImplicitPrelude #-}

module Util.Map
  ( Map
  , empty
  , insert
  , insertWith
  , member
  , lookup
  , fromList
  , depth
  , size
  , keys
  , toList
  ) where

import Prelude hiding (lookup)

data Balance = L | B | R
  deriving (Show)

data Map k v = Leaf | Node Balance (k,v) (Map k v) (Map k v)
  deriving (Show)

insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert = insertWith const

data HeightChange = Same | Grew

-- | Insert with the given merge function.  When the key
-- is already in the map, apply the merge function to the
-- new and existing values, and store the result.
--
insertWith :: (Ord k) => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v = snd . go
  where
  go Leaf =
    (Grew, Node B (k,v) Leaf Leaf)
  go (Node b (nk,nv) l r) =
    case compare k nk of
      EQ -> (Same, Node b (k, f v nv) l r)
      LT -> case (b, go l) of
        -- left child grew, double rotation required
        (L, (Grew, l'@(Node R _ _ _))) ->
          (Same, rotr (Node L (nk,nv) (rotl l') r))
        -- left child grew, simple rotation required
        (L, (Grew, l')) -> (Same, rotr (Node L (nk,nv) l' r))
        -- left child grew, unbalancing this node
        (B, (Grew, l')) -> (Grew, Node L (nk,nv) l' r)
        -- left child grew, children are now same hight
        (R, (Grew, l')) -> (Same, Node B (nk,nv) l' r)
        -- left child same height, balance preserved
        (_, (_,    l')) -> (Same, Node b (nk,nv) l' r)
      GT -> case (b, go r) of
        -- right child grew, children are now same hight
        (L, (Grew, r')) -> (Same, Node B (nk,nv) l r')
        -- right child grew, unbalancing this node
        (B, (Grew, r')) -> (Grew, Node R (nk,nv) l r')
        -- right child grew, double rotation required
        (R, (Grew, r'@(Node L _ _ _))) ->
          (Same, rotl (Node R (nk,nv) l (rotr r')))
        -- right child grew, simple rotation required
        (R, (Grew, r')) -> (Same, rotl (Node R (nk,nv) l r'))
        -- right child same height, balance preserved
        (_, (_,    r')) -> (Same, Node b (nk,nv) l r')

rotl :: Map k v -> Map k v
rotl (Node R kv l (Node rb rkv rl rr)) =
  let
    (b', rb') = case rb of B -> (L, R) ; _ -> (B, B)
  in
    Node b' rkv (Node rb' kv l rl) rr
rotl t = t

rotr :: Map k v -> Map k v
rotr (Node L kv (Node lb lkv ll lr) r) =
  let
    (b', lb') = case lb of B -> (R, L) ; _ -> (B, B)
  in
    Node b' lkv ll (Node lb' kv lr r)
rotr t = t

empty :: Map k v
empty = Leaf

lookup :: (Ord k) => k -> Map k v -> Maybe v
lookup _ Leaf = Nothing
lookup k (Node _ (nk,nv) l r) = case compare k nk of
  LT -> lookup k l
  GT -> lookup k r
  EQ -> Just nv

member :: (Ord k) => k -> Map k v -> Bool
member k = maybe False (const True) . lookup k

fromList :: (Ord k) => [(k,v)] -> Map k v
fromList = foldr (\(k,v) -> insert k v) empty

depth :: Map k v -> Int
depth Leaf = 0
depth (Node _ _ l r) = 1 + max (depth l) (depth r)

size :: Map k v -> Int
size Leaf = 0
size (Node _ _ l r) = 1 + size l + size r

keys :: Map k v -> [k]
keys = fmap fst . toList

toList :: Map k v -> [(k,v)]
toList Leaf = []
toList (Node _ (k,v) l r) = toList l <> ((k,v) : toList r)
