module Chapter01Test where

import Test.Tasty
import Test.Tasty.HUnit
import Chapter01

unit_doubleMeTest = doubleMe 2 @?= 4
