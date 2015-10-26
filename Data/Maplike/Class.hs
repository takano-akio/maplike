{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Maplike.Class (Maplike(..)) where

import Control.Applicative hiding (empty)
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntMap.Strict as IMS
import qualified Data.Map.Lazy as M
import qualified Data.Map.Strict as MS
import qualified Data.Maybe as Maybe
import Data.Monoid
import Prelude hiding (lookup)

class (Traversable m) => Maplike k m | m -> k where
  -- * Query
  member :: k -> m v -> Bool
  lookup :: k -> m v -> Maybe v

  -- * Construction
  empty :: m v
  singleton :: k -> v -> m v
  fromList :: [(k, v)] -> m v
  fromList' :: [(k, v)] -> m v

  -- ** Insertion
  insert :: k -> v -> m v -> m v
  insert = insertWith const
  insertWith :: (v -> v -> v) -> k -> v -> m v -> m v
  insertWith' ::  (v -> v -> v) -> k -> v -> m v -> m v

  -- ** Delete/Update
  delete :: k -> m v -> m v
  adjust :: (v -> v) -> k -> m v -> m v
  adjust f = update (Just . f)
  adjust' :: (v -> v) -> k -> m v -> m v
  update :: (v -> Maybe v) -> k -> m v -> m v
  alter :: (Maybe v -> Maybe v) -> k -> m v -> m v
  alterF :: Functor f => (Maybe v -> f (Maybe v)) -> k -> m v -> f (m v)
  alterF f k m = ins <$> f (lookup k m)
    where
      ins Nothing = m
      ins (Just v) = insert k v m

  -- * Combine
  mergeWithKey :: (k -> u -> v -> Maybe w) ->
    (m u -> m w) -> (m v -> m w) ->
    m u -> m v -> m w
  mergeWithKey' :: (k -> u -> v -> Maybe w) ->
    (m u -> m w) -> (m v -> m w) ->
    m u -> m v -> m w

  -- ** Union
  union :: m v -> m v -> m v
  union = unionWithKey (const const)
  unionWithKey :: (k -> v -> v -> v) -> m v -> m v -> m v
  unionWithKey' :: (k -> v -> v -> v) -> m v -> m v -> m v

  -- ** Difference
  difference :: m u -> m v -> m u
  differenceWithKey :: (k -> u -> v -> Maybe u) -> m u -> m v -> m u

  -- ** Intersection
  intersection :: m u -> m v -> m u
  intersectionWithKey :: (k -> u -> v -> w) -> m u -> m v -> m w
  intersectionWithKey' :: (k -> u -> v -> w) -> m u -> m v -> m w

  -- * Traversal
  traverseWithKey :: (Applicative f) => (k -> u -> f v) -> m u -> f (m v)
  traverseWithKey' :: (Applicative f) => (k -> u -> f v) -> m u -> f (m v)

  -- * Fold
  foldMapWithKey :: (Monoid n) => (k -> u -> n) -> m u -> n
  foldMapWithKey f = foldrWithKey (\k v r -> f k v <> r) mempty
  foldrWithKey :: (k -> u -> a -> a) -> a -> m u -> a
  foldrWithKey' :: (k -> u -> a -> a) -> a -> m u -> a
  foldlWithKey :: (a -> k -> u -> a) -> a -> m u -> a
  foldlWithKey' :: (a -> k -> u -> a) -> a -> m u -> a

  -- ** Map
  mapWithKey :: (k -> u -> v) -> m u -> m v
  mapWithKey' :: (k -> u -> v) -> m u -> m v

  -- ** Filter
  filterWithKey :: (k -> v -> Bool) -> m v -> m v
  filterWithKey f = mapMaybeWithKey (\k v -> if f k v then Just v else Nothing)

  -- ** MapMaybe
  mapMaybeWithKey :: (k -> u -> Maybe v) -> m u -> m v

------------------------------------------------------------------------

instance (Ord k) => Maplike k (M.Map k) where
  member = M.member
  lookup = M.lookup
  fromList = M.fromList
  fromList' = MS.fromList

  empty = M.empty
  singleton = M.singleton

  insert = M.insert
  insertWith = M.insertWith
  insertWith' = MS.insertWith

  delete = M.delete
  adjust = M.adjust
  adjust' = MS.adjust
  update = M.update
  alter = M.alter

  mergeWithKey = M.mergeWithKey
  mergeWithKey' = MS.mergeWithKey

  union = M.union
  unionWithKey = M.unionWithKey
  unionWithKey' = MS.unionWithKey

  difference = M.difference
  differenceWithKey = M.differenceWithKey

  intersection = M.intersection
  intersectionWithKey = M.intersectionWithKey
  intersectionWithKey' = MS.intersectionWithKey

  traverseWithKey = M.traverseWithKey
  traverseWithKey' = MS.traverseWithKey

  foldMapWithKey = M.foldMapWithKey
  foldrWithKey = M.foldrWithKey
  foldrWithKey' = M.foldrWithKey'
  foldlWithKey = M.foldlWithKey
  foldlWithKey' = M.foldlWithKey'

  mapWithKey = M.mapWithKey
  mapWithKey' = MS.mapWithKey

  filterWithKey = M.filterWithKey
  mapMaybeWithKey = M.mapMaybeWithKey

------------------------------------------------------------------------

instance Maplike Int IM.IntMap where
  member = IM.member
  lookup = IM.lookup
  fromList = IM.fromList
  fromList' = IMS.fromList

  empty = IM.empty
  singleton = IM.singleton

  insert = IM.insert
  insertWith = IM.insertWith
  insertWith' = IMS.insertWith

  delete = IM.delete
  adjust = IM.adjust
  adjust' = IMS.adjust
  update = IM.update
  alter = IM.alter

  mergeWithKey = IM.mergeWithKey
  mergeWithKey' = IMS.mergeWithKey

  union = IM.union
  unionWithKey = IM.unionWithKey
  unionWithKey' = IMS.unionWithKey

  difference = IM.difference
  differenceWithKey = IM.differenceWithKey

  intersection = IM.intersection
  intersectionWithKey = IM.intersectionWithKey
  intersectionWithKey' = IMS.intersectionWithKey

  traverseWithKey = IM.traverseWithKey
  traverseWithKey' = IMS.traverseWithKey

  foldMapWithKey = IM.foldMapWithKey
  foldrWithKey = IM.foldrWithKey
  foldrWithKey' = IM.foldrWithKey'
  foldlWithKey = IM.foldlWithKey
  foldlWithKey' = IM.foldlWithKey'

  mapWithKey = IM.mapWithKey
  mapWithKey' = IMS.mapWithKey

  filterWithKey = IM.filterWithKey
  mapMaybeWithKey = IM.mapMaybeWithKey

------------------------------------------------------------------------

instance (Eq k, Hashable k) => Maplike k (HM.HashMap k) where
  member = HM.member
  lookup = HM.lookup

  empty = HM.empty
  singleton = HM.singleton
  fromList = HM.fromList
  fromList' = HMS.fromList

  insert = HM.insert
  insertWith = HM.insertWith
  insertWith' = HMS.insertWith

  delete = HM.delete
  adjust = HM.adjust
  adjust' = HMS.adjust
  -- | 'HM.HashMap' has no efficient implementation of 'update'.
  update f k m = maybe m
    (maybe (delete k m) (insert k `flip` m) . f) (lookup k m)
  -- | 'HM.HashMap' has no efficient implementation of 'alter'.
  alter f k m = maybe (delete k m) (insert k `flip` m) (f $ lookup k m)

  -- | 'HM.HashMap' has no efficient implementation of 'mergeWithKey'.
  mergeWithKey uvw muw mvw mu mv
    = maebe (HM.intersectionWith ($) (HM.mapWithKey uvw mu) mv)
      `HM.union` muw (HM.difference mu mv)
      `HM.union` mvw (HM.difference mv mu) where
    maebe = HM.fromList . Maybe.mapMaybe (\ (k, mw) -> (,) k <$> mw) . HM.toList
  mergeWithKey' uvw muw mvw mu mv
    = maebe (HMS.intersectionWith ($) (HMS.mapWithKey uvw mu) mv)
      `HM.union` muw (HMS.difference mu mv)
      `HM.union` mvw (HMS.difference mv mu) where
    maebe = HMS.fromList . Maybe.mapMaybe (\ (k, mw) -> (,) k <$> mw) . HMS.toList

  union = HM.union
  -- | 'HM.HashMap' has no efficient implementation of 'unionWithKey'.
  unionWithKey f mu mv = mergeWithKey (\ k u v -> Just $ f k u v) id id mu mv
  unionWithKey' f mu mv = mergeWithKey' (\ k u v -> Just $ f k u v) id id mu mv

  difference = HM.difference
  -- differenceWith
  differenceWithKey f = mergeWithKey f id (const empty)

  intersection = HM.intersection
  -- intersectionWith
  intersectionWithKey f = mergeWithKey
    (\ k u v -> Just $ f k u v) (const empty) (const empty)
  intersectionWithKey' f = mergeWithKey'
    (\ k u v -> Just $ f k u v) (const empty) (const empty)

  traverseWithKey = HM.traverseWithKey
  traverseWithKey' = HMS.traverseWithKey

  foldrWithKey = HM.foldrWithKey
  foldrWithKey' f = HM.foldrWithKey $ \k v r -> f k v $! r
  foldlWithKey f x t = HM.foldrWithKey (\k v r a -> r (f a k v)) id t x
  foldlWithKey' f x t = HM.foldrWithKey (\k v r a -> r $! f a k v) id t x

  mapWithKey = HM.mapWithKey
  mapWithKey' = HMS.mapWithKey

  filterWithKey = HM.filterWithKey
  -- mapMaybe
  -- | 'HM.HashMap' has no efficient implementation of 'mapMaybeWithKey'.
  mapMaybeWithKey f
    = HM.map Maybe.fromJust . HM.filter Maybe.isJust . HM.mapWithKey f
