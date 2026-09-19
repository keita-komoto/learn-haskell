module Chapter06 where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M

-- | Remove duplicates on the list
-- >>> numUniques [1,2,1,3,1,4]
-- 4
numUniques :: Eq a => [a] -> Int
numUniques = length . L.nub

wawee = "wa wa wee wa wee"

-- | Count the frequency of words
-- >>> wordNum wawee
-- [("wa",3),("wee",2)]
wordNum :: String -> [(String, Int)]
wordNum = map (\ws -> (head ws, length ws)) . L.group . L.sort . L.words

-- | Find something in array
-- >>> "art" `isIn` "party"
-- True
-- >>> [1,2] `isIn` [1,3,5]
-- False
isIn :: Eq a => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `L.isPrefixOf`) $ L.tails haystack

-- | Use a Caesar cipher
-- >>> encode 3 "hey mark"
-- "kh|#pdun"
encode :: Int -> String -> String
encode offset = map (\c -> C.chr $ C.ord c + offset)

-- | Decode a Caesar cipher
-- >>> decode 3 "kh|#pdun"
-- "hey mark"
decode :: Int -> String -> String
decode shift = encode $ negate shift

-- | Returns the sum of the digits
-- >>> digitSum 12345
-- 15
digitSum :: Int -> Int
digitSum = sum . map C.digitToInt . show

-- | Find the first number whose digits add up to N
-- >>> firstTo 13
-- Just 49
firstTo :: Int -> Maybe Int
firstTo n = L.find (\x -> digitSum x == n) [1 ..]

phoneBook =
  [ ("betty", "111-2222"),
    ("bonnie", "222-3333"),
    ("patsy", "333-4444"),
    ("lucille", "444-5555"),
    ("wendy", "555-6666"),
    ("penny", "666-7777")
  ]

phoneBook' :: M.Map String String
phoneBook' =
  M.fromList
    [ ("betty", "111-2222"),
      ("bonnie", "222-3333"),
      ("patsy", "333-4444"),
      ("lucille", "444-5555"),
      ("wendy", "555-6666"),
      ("penny", "666-7777")
    ]

-- Find a key from an association list
-- >>> findKey "betty" phoneBook
-- Just "111-2222"
-- >>> findKey "macho" phoneBook
-- Nothing
findKey :: Eq k => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

-- | Convert the string of a phone number into an Int list
-- >>> stringToDigits "948-9282"
-- [9,4,8,9,2,8,2]
-- >>> M.lookup "betty" $ M.map stringToDigits phoneBook'
-- Just [1,1,1,2,2,2,2]
stringToDigits :: String -> [Int]
stringToDigits = map C.digitToInt . filter C.isDigit
