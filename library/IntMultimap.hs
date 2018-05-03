module IntMultimap
(
  IntMultiMap,

  -- * Construction
  empty,
  singleton,

  -- * List
  toList,
  fromList,

  -- * Transformations
  map,

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
newtype IntMultiMap a =
  IntMultiMap (A.IntMap (B.HashSet a))
  deriving(Foldable, Eq, Show, Generic)

{--------------------------------------------------------------------
  Transformations
--------------------------------------------------------------------}
map :: (Eq b, Hashable b) => (a -> b) -> IntMultiMap a -> IntMultiMap b
map f (IntMultiMap intMap) = IntMultiMap $
  fmap (\hashSet -> B.map f hashSet) intMap
{-# INLINE map #-}

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
instance (Eq a, Hashable a) => G.IsList (IntMultiMap a) where
  type Item (IntMultiMap a) = (Int, a)
  toList = toList
  fromList = fromList

toList :: IntMultiMap a -> [(Int, a)]
toList (IntMultiMap multiMap) = do
  (key, hashSet) <- A.toList multiMap
  fmap ((,) key) $ B.toList hashSet

fromList :: (Eq a, Hashable a) =>
     [(Int, a)] -> IntMultiMap a
fromList = IntMultiMap . A.fromListWith B.union . fmap (fmap B.singleton)

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
empty :: IntMultiMap a
empty = IntMultiMap A.empty

singleton :: (Hashable a) => Int -> a -> IntMultiMap a
singleton k v = IntMultiMap $ A.singleton k $ B.singleton v
{-# INLINABLE singleton #-}

{--------------------------------------------------------------------
  Basic interface
--------------------------------------------------------------------}
null :: IntMultiMap a -> Bool
null (IntMultiMap intMap) = A.null intMap
{-# INLINE null #-}

size :: IntMultiMap a -> Int
size = length . toList

member :: Int -> IntMultiMap a -> Bool
member key (IntMultiMap intMap) = A.member key intMap

insert :: (Hashable a, Ord a) => Int -> a -> IntMultiMap a -> IntMultiMap a
insert key value (IntMultiMap intMap) =
  IntMultiMap $ A.insertWith (f value) key (B.singleton value) intMap
    where
      f v new old = B.insert v old

delete :: (Hashable a, Eq a) => Int {-^ Key -} -> a -> IntMultiMap a -> IntMultiMap a
delete key value (IntMultiMap intMap) =
  IntMultiMap $ A.update f key intMap
  where
    f hashSet =
      mfilter (not . B.null) . Just $ B.delete value hashSet

{--------------------------------------------------------------------
  Conversions
--------------------------------------------------------------------}
elems :: IntMultiMap a -> [a]
elems = foldr (:) []

keys  :: IntMultiMap a -> [Int]
keys (IntMultiMap intMap) = A.keys intMap

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
split :: Int -> IntMultiMap a -> (IntMultiMap a, IntMultiMap a)
split key (IntMultiMap intMap) = (IntMultiMap oldMap, IntMultiMap newMap)
  where
    (oldMap, newMap) = A.split key intMap

splitLookup :: Int -> IntMultiMap a -> (IntMultiMap a, Maybe (B.HashSet a), IntMultiMap a)
splitLookup key (IntMultiMap intMap) = (IntMultiMap oldMap, elemHashSet, IntMultiMap newMap)
  where
    (oldMap, elemHashSet, newMap) = A.splitLookup key intMap
