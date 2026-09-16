module Chapter06Test where

import Chapter06
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.HUnit

genInt :: Gen Int
genInt = Gen.int (Range.constant (-10) 10)

unit_numUniquesEmpty = numUniques ([] :: [Int]) @?= 0

hprop_numUniquesAddingExistingValuesMakesNoDifference = property $ do
  xs <- forAll $ Gen.list (Range.linear 1 100) genInt
  x <- forAll $ Gen.element xs
  numUniques (x : xs) === numUniques xs

hprop_numUniquesIncreasesWhenAddNewValue = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) genInt
  x <- forAll genInt
  let ys = filter (/= x) xs
  numUniques (x : ys) === numUniques ys + 1
