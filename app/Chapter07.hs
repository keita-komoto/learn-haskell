module Chapter07 where

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving Show

-- | Display the string of the calculation formula
-- >>> Circle 10 20 10
-- Circle 10.0 20.0 10.0

-- | Calculate the area of a circle
-- >>> area $ Circle 10 20 10
-- 314.15927

-- | Calculate the area of a rectangle
-- >>> area $ Rectangle 0 0 100 100
-- 10000.0
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
