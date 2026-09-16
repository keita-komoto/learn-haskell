module Chapter06 where

import qualified Data.Char as C
import qualified Data.List as L

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
encode offset message = map (\c -> C.chr $ C.ord c + offset) message

-- | Decode a Caesar cipher
-- >>> decode 3 "kh|#pdun"
-- "hey mark"
decode :: Int -> String -> String
decode shift message = encode (negate shift) message
