module Chapter06 where

import qualified Data.List as L

-- | Remove duplicates on the list
-- >>> numUniques [1,2,1,3,1,4]
-- 4
numUniques :: Eq a => [a] -> Int
numUniques = length . L.nub
