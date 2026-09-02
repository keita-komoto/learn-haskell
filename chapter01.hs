module Chapter01 where

doubleMe x = x+x
doubleUs x y = x*2 + y*2
doubleSmallNumber x = if x > 100 then x else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

lostNumbers = [4,8,15,16,23,42]

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

nouns = ["hobo","frog","pope"]
adjectives = ["lazy","grouchy","scheming"]
phrases = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2]]
removeOdd xs = [ [x | x <- xxs, even x] | xxs <- xs]

triples = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
