module Chapter05Test where

import Chapter05
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)

unit_quicksortIsMinOrderInt = quicksort randomIntItems @?= [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

unit_quicksortIsMinOrderString = quicksort stringItems @?= ["the quick brown fox jumps over the lazy dog"]

hprop_quicksortIsMinOrderInt = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linearFrom 0 (-100) 100))
  let result = quicksort xs
  assert (and (zipWith (<=) result (drop 1 result)))

hprop_quicksortIsMinOrderString = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.list (Range.linear 0 100) Gen.alpha)
  let result = quicksort xs
  assert (and (zipWith (<=) result (drop 1 result)))

unit_compareWithHundred = do
  compareWithHundred 100 @?= EQ
  compareWithHundred 99 @?= GT
  compareWithHundred 101 @?= LT

unit_zipWith' = do
  zipWith' (+) [1, 2, 3] [4, 5, 6] @?= [5, 7, 9]
  zipWith' (*) [1, 2, 3] [4, 5, 6] @?= [4, 10, 18]
  zipWith' (++) ["a", "b", "c"] ["d", "e", "f"] @?= ["ad", "be", "cf"]
  zipWith' max [1, 2, 3] [4, 5, 6] @?= [4, 5, 6]
  zipWith' (*) (replicate 5 2) [1 ..] @?= [2, 4, 6, 8, 10]
  zipWith' (zipWith' (*)) [[1, 2, 3], [3, 5, 6], [2, 3, 4]] [[3, 2, 2], [3, 4, 5], [5, 4, 3]] @?= [[3, 4, 6], [9, 20, 30], [10, 12, 12]]
  zipWith' (\a b -> (a * 30 + 3) / b) [5, 4, 3, 2, 1] [1, 2, 3, 4, 5] @?= [153.0, 61.5, 31.0, 15.75, 6.6]

unit_flip' = do
  flip' zip [1, 2, 3, 4, 5] "hello" @?= [('h', 1), ('e', 2), ('l', 3), ('l', 4), ('o', 5)]
  flip' (flip' zip) [1, 2, 3, 4, 5] "hello" @?= [(1, 'h'), (2, 'e'), (3, 'l'), (4, 'l'), (5, 'o')]
  zipWith' (flip' div) [2, 2 ..] [10, 8, 6, 4, 2] @?= [5, 4, 3, 2, 1]
  flip' zip [1, 2, 3, 4, 5] "hello" @?= flip'' zip [1, 2, 3, 4, 5] "hello"

unit_map = do
  map' (+ 3) [1, 5, 3, 1, 6] @?= [4, 8, 6, 4, 9]
  map' (++ "!") ["BIFF", "BANG", "POW"] @?= ["BIFF!", "BANG!", "POW!"]
  map' (replicate 3) [3 .. 6] @?= [[3, 3, 3], [4, 4, 4], [5, 5, 5], [6, 6, 6]]
  map' (map' (^ 2)) [[1, 2], [3, 4, 5, 6], [7, 8]] @?= [[1, 4], [9, 16, 25, 36], [49, 64]]
  map' fst [(1, 2), (3, 5), (6, 3), (2, 6), (2, 5)] @?= [1, 3, 6, 2, 2]

unit_filter' = do
  filter' even [1 .. 10] @?= [2, 4, 6, 8, 10]
  filter' (`elem` ['A' .. 'Z']) "iLAuGh at you bEcause uR all the same" @?= "LAGER"
  filter' (< 15) (filter' even [1 .. 20]) @?= [2, 4, 6, 8, 10, 12, 14]

unit_quicksortEqualQuicksortWithFilter = do
  quicksort randomIntItems @?= quicksortWithFilter randomIntItems
  quicksort stringItems @?= quicksortWithFilter stringItems

unit_largestDivisible = largestDivisible @?= 99554

unit_chain = do
  chain 1 @?= [1]
  chain 10 @?= [10, 5, 16, 8, 4, 2, 1]
