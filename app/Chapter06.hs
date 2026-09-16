module Chapter06 where

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
