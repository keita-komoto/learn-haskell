module Chapter05 where

randomIntItems = [10, 2, 3, 5, 1, 4, 6, 9, 8, 7]

stringItems = ["the quick brown fox jumps over the lazy dog"]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

-- | Compare a number with 100
-- >>> compareWithHundred 100
-- EQ
-- >>> compareWithHundred 99
-- GT
-- >>> compareWithHundred 101
-- LT
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

-- | Find the largest number under 100000 that is divisible by 3829
-- >>> largestDivisible
-- 99554
largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

-- | Generate the Collatz sequence for a given number
-- >>> chain 1
-- [1]
-- >>> chain 10
-- [10,5,16,8,4,2,1]
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

-- | Count how many Collatz sequences for numbers 1 to 100 have a length greater than 15
-- >>> numLongChains
-- 66
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs > 15
