module Rerun (rerun) where

import Test.Tasty (defaultIngredients)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Ingredients.Rerun (rerunningTests)

rerun :: Ingredient
rerun = rerunningTests defaultIngredients
