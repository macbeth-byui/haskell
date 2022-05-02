take' :: (Num a, Eq a) => a -> [b] -> [b]
take' 0 _ = []
take' n [] = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: (Num a, Eq a) => a -> [b] -> [b]
drop' 0 list = list
drop' n [] = []
drop' n (_:xs) = drop' (n-1) xs

length' :: Num b => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum_ :: Num a => [a] -> a
sum_ [] = 0
sum_ (first:rest) = first + sum_ rest

append :: [a] -> [a] -> [a]
append [] list = list
append (first:rest) list = first : append rest list

reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ (first:rest) = first : reverse_ rest

evenElements :: (Num a) => [a] -> [a]
evenElements (_:x:xs) = x : evenElements xs
evenElements _ = []

countTrue :: (Num a) => [Bool] -> a
-- countTrue [] = 0
-- countTrue (True:xs) = 1 + countTrue xs
-- countTrue (False:xs) = countTrue xs
countTrue l = length' [x | x <- l, x]

palindrome :: [a] -> [a]
palindrome l = l ++ reverse l

isPalindrome :: (Eq a) => [a] -> Bool 
isPalindrome l = l == reverse l

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x:xs) = x : dropLast xs

elem' :: (Eq a) => a -> [a] -> Bool 
elem' _ [] = False 
elem' v (x:xs) | v == x    = True 
               | otherwise = elem' v xs

makeSet :: (Eq a) => [a] -> [a]
makeSet [] = []
makeSet (x:xs) | elem' x xs = makeSet xs
               | otherwise  = x : makeSet xs


