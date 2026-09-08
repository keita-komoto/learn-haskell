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
