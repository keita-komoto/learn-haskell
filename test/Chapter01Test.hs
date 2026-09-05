module Chapter01Test where

import Chapter01
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit

unit_doubleMeTest = doubleMe 2 @?= 4

hprop_alwaysDoubled :: Property
hprop_alwaysDoubled = property $ do
  x <- forAll $ Gen.int (Range.linearFrom 0 (-100) 100)
  doubleMe x === 2 * x
