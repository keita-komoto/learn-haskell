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

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

quicksortWithFilter :: Ord a => [a] -> [a]
quicksortWithFilter [] = []
quicksortWithFilter (x : xs) =
  let smallerSorted = quicksortWithFilter (filter' (<= x) xs)
      largerSorted = quicksortWithFilter (filter' (> x) xs)
  in smallerSorted ++ [x] ++ largerSorted
