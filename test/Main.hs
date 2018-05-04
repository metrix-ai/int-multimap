module Main where

import Prelude hiding (first, second)
import Control.Arrow
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
-- import Test.QuickCheck.Instances
import qualified IntMultimap as A
import qualified Data.List as B
import Data.Maybe
import Data.Foldable as F
import qualified Data.HashSet as B

main =
  defaultMain $
  testGroup "All tests" $
  [
    testCase "Member" $ do
      let
        list = [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')]
        multimap = A.fromList list
      assertEqual "" True $ A.member 3 multimap
      assertEqual "" False $ A.member 5 multimap
    ,
    testCase "Lists" $ do
      let
        list = [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')]
        multimap = A.fromList list
      assertEqual "" list $ A.toList $ A.fromList list
      assertEqual "" multimap $ A.fromList $ A.toList multimap
    ,
    testCase "Conversions" $ do
      let
        list = [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')]
        multimap = A.fromList list
      assertEqual "" (map fst list) $ A.keys multimap
      assertEqual "" (map snd list) $ A.elems multimap
    ,
    testCase "Filter" $ do
      let
        list = [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')]
        multimap = A.fromList list
        (old, maybeElem, new) = A.splitLookup 3 multimap
      assertEqual "" ((A.toList old), [(3, 'c')] ++ A.toList new) (B.span (\(k,a) -> k < 3) list)
    ,
    testCase "Insert and Delete" $ do
      let
        list = [(1, 'a'), (2, 'b'), (4, 'd'), (5, 'e')]
        key = 3
        el = 'c'
        list2 = [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (5, 'e')]
      assertEqual "" list2 $ A.toList $ A.insert key el $ A.fromList list
      assertEqual "" list $ A.toList $ A.delete key el $ A.fromList list2
    ,
    testCase "singleton and empty" $ do
      let
        list = [(1, 'a')]
        multimap = A.fromList list
      assertEqual "" True $ A.null A.empty
      assertEqual "" list $ A.toList $ A.singleton 1 'a'
      assertEqual "" (A.toList multimap) $ A.toList $ A.singleton 1 'a'
    ,
    testProperty "list to list(Int, String)" $ \ (list :: [(Int, String)]) ->
    (B.fromList list) === (B.fromList $ A.toList $ A.fromList list)
    ,
    testProperty "list to list(Int, Int)" $ \ (list :: [(Int, Int)]) ->
    (B.fromList list) === (B.fromList $ A.toList $ A.fromList list)
    ,
    testProperty "insert and delete" $ \ (list :: [(Int, Char)], key :: Int, value :: Char) ->
    (B.fromList list) === (B.fromList $ A.toList $ A.delete key value $ A.insert key value $ A.fromList list)
    ,
    testProperty "Mapping" $ \ (list :: [(Int, Int)]) ->
    (B.fromList $ map testFuncL list) === (B.fromList $ A.toList $ A.map testFunc $ A.fromList list)
  ]

testFunc :: (Show a) => a -> String
testFunc = show

testFuncL :: (Show a)  => (Int, a) -> (Int, String)
testFuncL (a, b) = (a, show b)
