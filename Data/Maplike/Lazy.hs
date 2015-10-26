module Data.Maplike.Lazy where

import Data.Maplike.Class(Maplike)
import qualified Data.Maplike.Class as C

-- * Query

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
singleton = C.singleton
{-# INLINE singleton #-}

fromList :: (Maplike k m) => [(k, v)] -> m v
fromList = C.fromList
{-# INLINE fromList #-}

-- ** Insertion
insert :: (Maplike k m) => k -> v -> m v -> m v
insert = C.insert
{-# INLINE insert #-}

insertWith :: (Maplike k m) => (v -> v -> v) -> k -> v -> m v -> m v
insertWith = C.insertWith
{-# INLINE insertWith #-}

-- ** Delete/Update
delete :: (Maplike k m) => k -> m v -> m v
delete = C.delete
{-# INLINE delete #-}

adjust :: (Maplike k m) => (v -> v) -> k -> m v -> m v
adjust = C.adjust
{-# INLINE adjust #-}

update :: (Maplike k m) => (v -> Maybe v) -> k -> m v -> m v
update = C.update
{-# INLINE update #-}

alter :: (Maplike k m) => (Maybe v -> Maybe v) -> k -> m v -> m v
alter = C.alter
{-# INLINE alter #-}

alterF :: (Maplike k m) => Functor f => (Maybe v -> f (Maybe v)) -> k -> m v -> f (m v)
alterF = C.alterF
{-# INLINE alterF #-}

-- * Combine
mergeWithKey :: (Maplike k m) => (k -> u -> v -> Maybe w) ->
  (m u -> m w) -> (m v -> m w) ->
  m u -> m v -> m w
mergeWithKey = C.mergeWithKey
{-# INLINE mergeWithKey #-}

-- ** Union
union :: (Maplike k m) => m v -> m v -> m v
union = C.union
{-# INLINE union #-}

unionWith :: (Maplike k m) => (v -> v -> v) -> m v -> m v -> m v
unionWith = unionWithKey . const
{-# INLINE unionWith #-}

unionWithKey :: (Maplike k m) => (k -> v -> v -> v) -> m v -> m v -> m v
unionWithKey = C.unionWithKey
{-# INLINE unionWithKey #-}

-- ** Difference
difference :: (Maplike k m) => m u -> m v -> m u
difference = C.difference
{-# INLINE difference #-}

differenceWith :: (Maplike k m) => (u -> v -> Maybe u) -> m u -> m v -> m u
differenceWith = differenceWithKey . const
{-# INLINE differenceWith #-}

differenceWithKey :: (Maplike k m) => (k -> u -> v -> Maybe u) -> m u -> m v -> m u
differenceWithKey = C.differenceWithKey
{-# INLINE differenceWithKey #-}

-- ** Intersection
intersection :: (Maplike k m) => m u -> m v -> m u
intersection = C.intersection
{-# INLINE intersection #-}

intersectionWith :: (Maplike k m) => (u -> v -> w) -> m u -> m v -> m w
intersectionWith = intersectionWithKey . const
{-# INLINE intersectionWith #-}

intersectionWithKey :: (Maplike k m) => (k -> u -> v -> w) -> m u -> m v -> m w
intersectionWithKey = C.intersectionWithKey
{-# INLINE intersectionWithKey #-}

-- * Traversal
traverse :: (Maplike k m, Applicative f) => (u -> f v) -> m u -> f (m v)
traverse = traverseWithKey . const
{-# INLINE traverse #-}

traverseWithKey :: (Maplike k m, Applicative f) => (k -> u -> f v) -> m u -> f (m v)
traverseWithKey = C.traverseWithKey
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

-- ** Map
map :: (Maplike k m) => (u -> v) -> m u -> m v
map = mapWithKey . const
{-# INLINE map #-}

mapWithKey :: (Maplike k m) => (k -> u -> v) -> m u -> m v
mapWithKey = C.mapWithKey
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
mapMaybeWithKey = C.mapMaybeWithKey
{-# INLINE mapMaybeWithKey #-}
