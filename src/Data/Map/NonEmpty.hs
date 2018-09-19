{-# LANGUAGE DeriveFunctor #-}

module Data.Map.NonEmpty
  ( NonEmptyMap
  , singleton
  , lookup
  , foldl1'
  , foldr1'
  , mapWithKey
  , toNonEmpty
  , toMap
  ) where

import Prelude hiding (lookup,foldr1,foldr)
import Data.Bool (bool)
import Data.Semigroup.Foldable (Foldable1)
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Semigroup.Foldable

-- | A non-empty map.
data NonEmptyMap k v = NonEmptyMap !k v !(M.Map k v)
  deriving (Eq,Ord,Functor)

-- | A map with a single element.
singleton :: k -> v -> NonEmptyMap k v
singleton k v = NonEmptyMap k v M.empty

-- | Lookup the value at a key in the map.
lookup :: Ord k => k -> NonEmptyMap k a -> Maybe a
lookup a (NonEmptyMap k v xs) = maybe
  (bool Nothing (Just v) (a == k))
  Just
  (M.lookup a xs)

unionAppend :: (Ord k, Semigroup v)
  => NonEmptyMap k v -> NonEmptyMap k v -> NonEmptyMap k v
unionAppend (NonEmptyMap xk xv xs) (NonEmptyMap yk yv ys) = case compare xk yk of
  EQ -> NonEmptyMap xk (xv <> yv) (MS.unionWith (<>) xs ys)
  LT -> NonEmptyMap xk xv (MS.unionWith (<>) xs (M.insertWith (<>) yk yv ys))
  GT -> NonEmptyMap yk yv (MS.unionWith (<>) (M.insertWith (<>) xk xv xs) ys)

foldr1 :: (v -> v -> v) -> NonEmptyMap k v -> v
foldr1 f (NonEmptyMap _ v m) = case M.maxView m of
  Nothing -> v
  Just (y,m') -> f v (M.foldr f y m')

foldMap1 :: Semigroup m => (v -> m) -> NonEmptyMap k v -> m
foldMap1 f (NonEmptyMap _ v m) = case M.maxView m of
  Nothing -> f v
  Just (x,m') -> f v <> M.foldr (\c d -> f c <> d) (f x) m'

instance (Ord k, Semigroup v) => Semigroup (NonEmptyMap k v) where
  (<>) = unionAppend

instance Foldable (NonEmptyMap k) where
  fold (NonEmptyMap _ v m) = v <> F.fold m
  foldMap f (NonEmptyMap _ v m) = f v <> foldMap f m
  foldl f a (NonEmptyMap _ v m) = M.foldl f (f a v) m
  foldr f a (NonEmptyMap _ v m) = case M.maxView m of
    Nothing -> f v a
    Just (y,m') -> f v (M.foldr f (f y a) m')
  foldl1 f (NonEmptyMap _ v m) = M.foldl f v m
  foldr1 = foldr1
  length (NonEmptyMap _ _ m) = 1 + M.size m
  null _ = False
  minimum = foldl1' min
  maximum = foldl1' max

instance Foldable1 (NonEmptyMap k) where
  toNonEmpty = elems
  fold1 = foldr1 (<>)
  foldMap1 = foldMap1

elems :: NonEmptyMap k v -> NonEmpty v
elems (NonEmptyMap _ v m) = v :| M.elems m

foldl1' :: (v -> v -> v) -> NonEmptyMap k v -> v
foldl1' f (NonEmptyMap _ v m) = M.foldl' f v m

foldr1' :: (v -> v -> v) -> NonEmptyMap k v -> v
foldr1' f (NonEmptyMap _ v m) = case M.maxView m of
  Nothing -> v
  Just (y,m') -> let !k = M.foldr' f y m' in f v k

mapWithKey :: (k -> v -> w) -> NonEmptyMap k v -> NonEmptyMap k w
mapWithKey f (NonEmptyMap k v m) = NonEmptyMap k (f k v) (M.mapWithKey f m)

toNonEmpty :: NonEmptyMap k v -> NonEmpty (k,v)
toNonEmpty (NonEmptyMap k v m) = (k,v) :| M.toList m

toMap :: Ord k => NonEmptyMap k v -> M.Map k v
toMap (NonEmptyMap k v m) = M.insert k v m
