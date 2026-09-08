module Chapter05 where

randomIntItems = [10, 2, 3, 5, 1, 4, 6, 9, 8, 7]

stringItems = ["the quick brown fox jumps over the lazy dog"]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys
