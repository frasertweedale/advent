{-# LANGUAGE NoImplicitPrelude #-}

module Util.Map
  ( Map
  , empty
  , insert
  , member
  , lookup
  , fromList
  , depth
  ) where

import Prelude hiding (lookup)

data Map k v = Leaf | Node (k,v) (Map k v) (Map k v)
  deriving (Show)

insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert = insertWith const

-- | Insert with the given merge function.  When the key
-- is already in the map, apply the merge function to the
-- new and existing values, and store the result.
--
insertWith :: (Ord k) => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v Leaf = Node (k,v) Leaf Leaf
insertWith f k v (Node (nk,nv) l r) = case compare k nk of
  LT -> Node (nk,nv) (insertWith f k v l) r
  GT -> Node (nk,nv) l (insertWith f k v r)
  EQ -> Node (k, f v nv) l r

empty :: Map k v
empty = Leaf

lookup :: (Ord k) => k -> Map k v -> Maybe v
lookup _ Leaf = Nothing
lookup k (Node (nk,nv) l r) = case compare k nk of
  LT -> lookup k l
  GT -> lookup k r
  EQ -> Just nv

member :: (Ord k) => k -> Map k v -> Bool
member k = maybe False (const True) . lookup k

fromList :: (Ord k) => [(k,v)] -> Map k v
fromList = foldr (\(k,v) -> insert k v) empty

depth :: Map k v -> Int
depth Leaf = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)
