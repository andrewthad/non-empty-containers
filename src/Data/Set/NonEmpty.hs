module Data.Set.NonEmpty
  ( NonEmptySet
  , singleton
  , member
  , toSet
  , fromSet
  , toNonEmpty
  , fromNonEmpty
  ) where

import Prelude hiding (foldr1,foldr)

import Data.List.NonEmpty
import Data.Semigroup.Foldable (Foldable1)

import qualified Data.Set as S
import qualified Data.Foldable as F
import qualified Data.Semigroup.Foldable

-- | A non-empty set.
data NonEmptySet a = NonEmptySet !a !(S.Set a)
  deriving (Eq,Ord)

-- The internal invariant for a NonEmptySet is that the first
-- element in the NonEmptySet data constructor must be less
-- than everything in the Set that is the second argument.

-- | Create a non-empty set with a single element.
singleton :: a -> NonEmptySet a
singleton x = NonEmptySet x S.empty

-- | Convert a non-empty set to a non-empty list.
toNonEmpty :: NonEmptySet a -> NonEmpty a
toNonEmpty (NonEmptySet x xs) = x :| S.toList xs

-- | Is the element in the set?
member :: Ord a => a -> NonEmptySet a -> Bool
member a (NonEmptySet x xs) = S.member a xs || a == x

-- | Convert a non-empty set to a set.
toSet :: Ord a => NonEmptySet a -> S.Set a
-- We should be able to write this without an Ord constraint.
-- I cannot find anything in Data.Set.Internal that allows
-- me to do an unsafe insert on the left-hand side of a set. 
toSet (NonEmptySet x xs) = S.insert x xs

-- | Attempt to create a non-empty set from a set.
fromSet :: Ord a => S.Set a -> Maybe (NonEmptySet a)
fromSet s = fmap (uncurry NonEmptySet) (S.minView s)

-- | Create a non-empty set from a non-empty list.
fromNonEmpty :: Ord a => NonEmpty a -> NonEmptySet a
fromNonEmpty (x :| xs) = case S.minView s of
  Nothing -> NonEmptySet x S.empty
  Just (m,s') -> case compare x m of
    EQ -> NonEmptySet m s'
    GT -> NonEmptySet m (S.insert x s')
    LT -> NonEmptySet x s
  where
  s = S.fromList xs

foldr :: (a -> b -> b) -> b -> NonEmptySet a -> b
foldr f b (NonEmptySet a m) = case S.maxView m of
  Nothing -> f a b
  Just (y,m') -> f a (S.foldr f (f y b) m')

foldr1 :: (a -> a -> a) -> NonEmptySet a -> a
foldr1 f (NonEmptySet a s) = case S.maxView s of
  Nothing -> a
  Just (m,s') -> f a (S.foldr f m s')

foldMap1 :: Semigroup m => (a -> m) -> NonEmptySet a -> m
foldMap1 f (NonEmptySet a s) = case S.maxView s of
  Nothing -> f a
  Just (m,s') -> f a <> S.foldr (\c d -> f c <> d) (f m) s'

instance Show a => Show (NonEmptySet a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromNonEmpty " . shows (toNonEmpty xs)

instance Foldable NonEmptySet where
  fold (NonEmptySet a s) = a <> F.fold s
  foldMap f (NonEmptySet a s) = f a <> foldMap f s
  foldl1 f (NonEmptySet a s) = S.foldl f a s
  foldr1 = foldr1
  foldr = foldr
  minimum (NonEmptySet a _) = a
  maximum (NonEmptySet a s) = case S.lookupMax s of
    Nothing -> a
    Just m -> m
  length (NonEmptySet _ s) = 1 + S.size s
  null _ = False

instance Foldable1 NonEmptySet where
  toNonEmpty = toNonEmpty
  fold1 = foldr1 (<>)
  foldMap1 = foldMap1

instance Ord a => Semigroup (NonEmptySet a) where
  NonEmptySet x xs <> NonEmptySet y ys = case compare x y of
    EQ -> NonEmptySet x (xs <> ys)
    LT -> NonEmptySet x (xs <> S.insert y ys)
    GT -> NonEmptySet y (S.insert x xs <> ys)

