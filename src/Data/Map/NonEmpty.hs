{-# LANGUAGE DeriveFunctor #-}

module Data.Map.NonEmpty
  ( NonEmptyMap
  , singleton
  , lookup
  , foldl1'
  , foldr1'
  ) where

import Prelude hiding (lookup)
import Data.Bool (bool)
import Data.Foldable

import qualified Data.Map as M
import qualified Data.Map.Strict as MS

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

instance (Ord k, Semigroup v) => Semigroup (NonEmptyMap k v) where
  (<>) = unionAppend

instance Foldable (NonEmptyMap k) where
  fold (NonEmptyMap _ v m) = v <> fold m
  foldMap f (NonEmptyMap _ v m) = f v <> foldMap f m
  foldl f a (NonEmptyMap _ v m) = M.foldl f (f a v) m
  foldr f a (NonEmptyMap _ v m) = case M.maxView m of
    Nothing -> f v a
    Just (y,m') -> f v (M.foldr f (f y a) m')
  foldl1 f (NonEmptyMap _ v m) = M.foldl f v m
  foldr1 f (NonEmptyMap _ v m) = case M.maxView m of
    Nothing -> v
    Just (y,m') -> f v (M.foldr f y m')
  length (NonEmptyMap _ _ m) = 1 + M.size m
  null _ = False
  minimum = foldl1' min
  maximum = foldl1' max

foldl1' :: (v -> v -> v) -> NonEmptyMap k v -> v
foldl1' f (NonEmptyMap _ v m) = M.foldl' f v m

foldr1' :: (v -> v -> v) -> NonEmptyMap k v -> v
foldr1' f (NonEmptyMap _ v m) = case M.maxView m of
  Nothing -> v
  Just (y,m') -> let !k = M.foldr' f y m' in f v k
