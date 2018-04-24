module IntMultiMap
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
  split
)
where

import qualified Data.IntMap.Strict as A
import qualified Data.HashSet as B
import qualified Data.Foldable as C
import qualified GHC.Exts as G
import Prelude hiding (map, null)
import Data.Int
import Data.Hashable
import Data.Functor
import Data.List(length)
import Control.Monad

{-|
A multi map of integers to values a.
-}
newtype IntMultiMap value =
  IntMultiMap (A.IntMap (B.HashSet value))
  deriving(Foldable)

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
instance (Eq value, Hashable value) => G.IsList (IntMultiMap value) where
  type Item (IntMultiMap value) = (Int, value)
  toList = toList
  fromList = fromList

toList :: IntMultiMap b -> [(Int, b)]
toList (IntMultiMap multiMap) = do
  (key, hashSet) <- A.toList multiMap
  fmap ((,) key) $ B.toList hashSet

fromList :: (Eq value, Hashable value) =>
     [(Int, value)] -> IntMultiMap value
fromList = IntMultiMap . A.fromListWith B.union . fmap (fmap B.singleton)

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
empty :: IntMultiMap a
empty = IntMultiMap A.empty

singleton :: (Hashable value) => Int -> value -> IntMultiMap value
singleton k v = IntMultiMap $ A.singleton k $ B.singleton v
{-# INLINABLE singleton #-}

{--------------------------------------------------------------------
  Basic interface
--------------------------------------------------------------------}
null :: IntMultiMap value -> Bool
null (IntMultiMap intMap) = A.null intMap
{-# INLINE null #-}

size :: IntMultiMap value -> Int
size = length . toList

member :: Int -> IntMultiMap value -> Bool
member key (IntMultiMap intMap) = A.member key intMap

insert :: (Hashable value, Ord value) => Int -> value -> IntMultiMap value -> IntMultiMap value
insert key value (IntMultiMap intMap) =
  IntMultiMap $ A.update (\hash -> Just $ B.insert value hash) key intMap

delete :: (Hashable value, Eq value) => Int {-^ Key -} -> value -> IntMultiMap value -> IntMultiMap value
delete key value (IntMultiMap intMap) =
  IntMultiMap $ A.update f key intMap
  where
    f hashSet =
      mfilter (not . B.null) . Just $ B.delete value hashSet

{--------------------------------------------------------------------
  Conversions
--------------------------------------------------------------------}
elems :: IntMultiMap value -> [value]
elems = foldr (:) []

keys  :: IntMultiMap a -> [Int]
keys (IntMultiMap intMap) = A.keys intMap

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
split :: Int -> IntMultiMap value -> (IntMultiMap value, IntMultiMap value)
split key (IntMultiMap intMap) = (IntMultiMap oldMap, IntMultiMap newMap)
  where
    (oldMap, newMap) = A.split key intMap
