module IntMultimap
(
  IntMultimap,

  -- * Construction
  empty,
  singleton,

  -- * List
  toList,
  fromList,

  -- * Transformations
  map,

  -- * Folds
  foldlWithKey',

  -- * Basic interface
  null,
  size,
  member,
  delete,
  insert,

  -- * Conversions
  elems,
  keys,

  -- * Filter
  split,
  splitLookup
)
where

import qualified Data.IntMap.Strict as A
import qualified Data.HashSet as B
import qualified Data.Foldable as C
import qualified GHC.Exts as G
import GHC.Generics
import Prelude hiding (map, null)
import Data.Int
import Data.Hashable
import Data.Functor
import Data.List(length)
import Data.Maybe
import Control.Monad

{-|
A multi map of integers to values a.
-}
newtype IntMultimap a =
  IntMultimap (A.IntMap (B.HashSet a))
  deriving(Foldable, Eq, Show, Generic)

{--------------------------------------------------------------------
  Transformations
--------------------------------------------------------------------}
map :: (Eq b, Hashable b) => (a -> b) -> IntMultimap a -> IntMultimap b
map f (IntMultimap intMap) = IntMultimap $
  fmap (\hashSet -> B.map f hashSet) intMap
{-# INLINE map #-}

{--------------------------------------------------------------------
  Lists
-------------------------IntMultimap -------------------------------------------}
instance (Eq a, Hashable a) => G.IsList (IntMultimap a) where
  type Item (IntMultimap a) = (Int, a)
  toList = toList
  fromList = fromList

toList :: IntMultimap a -> [(Int, a)]
toList (IntMultimap multiMap) = do
  (key, hashSet) <- A.toList multiMap
  fmap ((,) key) $ B.toList hashSet

fromList :: (Eq a, Hashable a) =>
     [(Int, a)] -> IntMultimap a
fromList = IntMultimap . A.fromListWith B.union . fmap (fmap B.singleton)

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
empty :: IntMultimap a
empty = IntMultimap A.empty

singleton :: (Hashable a) => Int -> a -> IntMultimap a
singleton k v = IntMultimap $ A.singleton k $ B.singleton v
{-# INLINABLE singleton #-}

{--------------------------------------------------------------------
  Basic interface
--------------------------------------------------------------------}
null :: IntMultimap a -> Bool
null (IntMultimap intMap) = A.null intMap
{-# INLINE null #-}

size :: IntMultimap a -> Int
size = length . toList

member :: Int -> IntMultimap a -> Bool
member key (IntMultimap intMap) = A.member key intMap

insert :: (Hashable a, Eq a) => Int -> a -> IntMultimap a -> IntMultimap a
insert key value (IntMultimap intMap) =
  IntMultimap $ A.insertWith (f value) key (B.singleton value) intMap
    where
      f v new old = B.insert v old

delete :: (Hashable a, Eq a) => Int {-^ Key -} -> a -> IntMultimap a -> IntMultimap a
delete key value (IntMultimap intMap) =
  IntMultimap $ A.update f key intMap
  where
    f hashSet =
      mfilter (not . B.null) . Just $ B.delete value hashSet

{--------------------------------------------------------------------
  Conversions
--------------------------------------------------------------------}
elems :: IntMultimap a -> [a]
elems = foldr (:) []

keys  :: IntMultimap a -> [Int]
keys (IntMultimap intMap) = A.keys intMap

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
foldlWithKey' :: (Int -> a -> b -> a) -> a -> IntMultimap b -> a
foldlWithKey' f v (IntMultimap intMap) = A.foldlWithKey' (\a k set -> B.foldl' (f k) a set) v intMap

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
split :: Int -> IntMultimap a -> (IntMultimap a, IntMultimap a)
split key (IntMultimap intMap) = (IntMultimap oldMap, IntMultimap newMap)
  where
    (oldMap, newMap) = A.split key intMap

splitLookup :: Int -> IntMultimap a -> (IntMultimap a, Maybe (B.HashSet a), IntMultimap a)
splitLookup key (IntMultimap intMap) = (IntMultimap oldMap, elemHashSet, IntMultimap newMap)
  where
    (oldMap, elemHashSet, newMap) = A.splitLookup key intMap
