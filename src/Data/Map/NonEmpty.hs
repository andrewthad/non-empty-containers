module Data.Map.NonEmpty
  ( NonEmptyMap
  , singleton
  , lookup
  ) where

import Prelude hiding (lookup)
import Data.Bool (bool)

import qualified Data.Map as M
import qualified Data.Map.Strict as MS

-- | A non-empty map.
data NonEmptyMap k v = NonEmptyMap !k v !(M.Map k v)
  deriving (Eq,Ord)

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


