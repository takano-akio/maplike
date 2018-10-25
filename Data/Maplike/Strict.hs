{-# LANGUAGE BangPatterns #-}
module Data.Maplike.Strict
  ( Maplike, OrderedMaplike
  , module Data.Maplike.Strict
  ) where

import Control.Monad
import qualified Data.Foldable as Fold
import Data.Maplike.Class(Maplike, OrderedMaplike)
import qualified Data.Maplike.Class as C

import Prelude hiding(null)

-- * Query

size :: (Maplike k m) => m v -> Int
size = length
{-# INLINE size #-}

null :: (Maplike k m) => m v -> Bool
null = Fold.null
{-# INLINE null #-}

member :: (Maplike k m) => k -> m v -> Bool
member = C.member
{-# INLINE member #-}

lookup :: (Maplike k m) => k -> m v -> Maybe v
lookup = C.lookup
{-# INLINE lookup #-}

-- * Construction
empty :: (Maplike k m) => m v
empty = C.empty
{-# INLINE empty #-}

singleton :: (Maplike k m) => k -> v -> m v
singleton k !v = C.singleton k v
{-# INLINE singleton #-}

fromList :: (Maplike k m) => [(k, v)] -> m v
fromList = C.fromList'
{-# INLINE fromList #-}

-- ** Insertion
insert :: (Maplike k m) => k -> v -> m v -> m v
insert k !v m = C.insert k v m
{-# INLINE insert #-}

insertWith :: (Maplike k m) => (v -> v -> v) -> k -> v -> m v -> m v
insertWith = C.insertWith'
{-# INLINE insertWith #-}

-- ** Delete/Update
delete :: (Maplike k m) => k -> m v -> m v
delete = C.delete
{-# INLINE delete #-}

adjust :: (Maplike k m) => (v -> v) -> k -> m v -> m v
adjust = C.adjust'
{-# INLINE adjust #-}

update :: (Maplike k m) => (v -> Maybe v) -> k -> m v -> m v
update f = C.update ((return $!) <=< f)
{-# INLINE update #-}

alter :: (Maplike k m) => (Maybe v -> Maybe v) -> k -> m v -> m v
alter f = C.alter ((return $!) <=< f)
{-# INLINE alter #-}

alterF :: (Maplike k m) => Functor f => (Maybe v -> f (Maybe v)) -> k -> m v -> f (m v)
alterF f = C.alterF (fmap ((return $!) =<<) . f)
{-# INLINE alterF #-}

-- * Combine
mergeWithKey :: (Maplike k m) => (k -> u -> v -> Maybe w) ->
  (m u -> m w) -> (m v -> m w) ->
  m u -> m v -> m w
mergeWithKey = C.mergeWithKey'
{-# INLINE mergeWithKey #-}

-- ** Union
union :: (Maplike k m) => m v -> m v -> m v
union = C.union
{-# INLINE union #-}

unionWith :: (Maplike k m) => (v -> v -> v) -> m v -> m v -> m v
unionWith = unionWithKey . const
{-# INLINE unionWith #-}

unionWithKey :: (Maplike k m) => (k -> v -> v -> v) -> m v -> m v -> m v
unionWithKey = C.unionWithKey'
{-# INLINE unionWithKey #-}

-- ** Difference
difference :: (Maplike k m) => m u -> m v -> m u
difference = C.difference
{-# INLINE difference #-}

differenceWith :: (Maplike k m) => (u -> v -> Maybe u) -> m u -> m v -> m u
differenceWith = differenceWithKey . const
{-# INLINE differenceWith #-}

differenceWithKey :: (Maplike k m) => (k -> u -> v -> Maybe u) -> m u -> m v -> m u
differenceWithKey f = C.differenceWithKey (\k a b -> (return$!) =<< f k a b)
{-# INLINE differenceWithKey #-}

-- ** Intersection
intersection :: (Maplike k m) => m u -> m v -> m u
intersection = C.intersection
{-# INLINE intersection #-}

intersectionWith :: (Maplike k m) => (u -> v -> w) -> m u -> m v -> m w
intersectionWith = intersectionWithKey . const
{-# INLINE intersectionWith #-}

intersectionWithKey :: (Maplike k m) => (k -> u -> v -> w) -> m u -> m v -> m w
intersectionWithKey = C.intersectionWithKey'
{-# INLINE intersectionWithKey #-}

-- * Traversal
traverse :: (Maplike k m, Applicative f) => (u -> f v) -> m u -> f (m v)
traverse = traverseWithKey . const
{-# INLINE traverse #-}

traverseWithKey :: (Maplike k m, Applicative f) => (k -> u -> f v) -> m u -> f (m v)
traverseWithKey = C.traverseWithKey'
{-# INLINE traverseWithKey #-}

-- ** Fold
foldMapWithKey :: (Maplike k m, Monoid n) => (k -> u -> n) -> m u -> n
foldMapWithKey = C.foldMapWithKey
{-# INLINE foldMapWithKey #-}

foldrWithKey :: (Maplike k m) => (k -> u -> a -> a) -> a -> m u -> a
foldrWithKey = C.foldrWithKey
{-# INLINE foldrWithKey #-}

foldrWithKey' :: (Maplike k m) => (k -> u -> a -> a) -> a -> m u -> a
foldrWithKey' = C.foldrWithKey'
{-# INLINE foldrWithKey' #-}

foldlWithKey :: (Maplike k m) => (a -> k -> u -> a) -> a -> m u -> a
foldlWithKey = C.foldlWithKey
{-# INLINE foldlWithKey #-}

foldlWithKey' :: (Maplike k m) => (a -> k -> u -> a) -> a -> m u -> a
foldlWithKey' = C.foldlWithKey'
{-# INLINE foldlWithKey' #-}

toList :: (Maplike k m) => m u -> [(k, u)]
toList = C.toList
{-# INLINE toList #-}

keys :: (Maplike k m) => m u -> [k]
keys = C.keys
{-# INLINE keys #-}

elems :: (Maplike k m) => m u -> [u]
elems = Fold.toList
{-# INLINE elems #-}

-- ** Map
map :: (Maplike k m) => (u -> v) -> m u -> m v
map = mapWithKey . const
{-# INLINE map #-}

mapWithKey :: (Maplike k m) => (k -> u -> v) -> m u -> m v
mapWithKey = C.mapWithKey'
{-# INLINE mapWithKey #-}

-- ** Filter
filter :: (Maplike k m) => (v -> Bool) -> m v -> m v
filter = filterWithKey . const
{-# INLINE filter #-}

filterWithKey :: (Maplike k m) => (k -> v -> Bool) -> m v -> m v
filterWithKey = C.filterWithKey
{-# INLINE filterWithKey #-}

-- ** MapMaybe
mapMaybe :: (Maplike k m) => (u -> Maybe v) -> m u -> m v
mapMaybe = mapMaybeWithKey . const
{-# INLINE mapMaybe #-}

mapMaybeWithKey :: (Maplike k m) => (k -> u -> Maybe v) -> m u -> m v
mapMaybeWithKey f = C.mapMaybeWithKey $ \k v -> (return $!) =<< f k v
{-# INLINE mapMaybeWithKey #-}

-- * Ordered lookup
lookupLT :: (OrderedMaplike k m) => k -> m v -> Maybe (k, v)
lookupLT = C.lookupLT
{-# INLINE lookupLT #-}

lookupGT :: (OrderedMaplike k m) => k -> m v -> Maybe (k, v)
lookupGT = C.lookupGT
{-# INLINE lookupGT #-}

lookupLE :: (OrderedMaplike k m) => k -> m v -> Maybe (k, v)
lookupLE = C.lookupLE
{-# INLINE lookupLE #-}

lookupGE :: (OrderedMaplike k m) => k -> m v -> Maybe (k, v)
lookupGE = C.lookupGE
{-# INLINE lookupGE #-}

-- * Min/Max
minViewWithKey :: (OrderedMaplike k m) => m v -> Maybe ((k, v), m v)
minViewWithKey = C.minViewWithKey
{-# INLINE minViewWithKey #-}

maxViewWithKey :: (OrderedMaplike k m) => m v -> Maybe ((k, v), m v)
maxViewWithKey = C.maxViewWithKey
{-# INLINE maxViewWithKey #-}
