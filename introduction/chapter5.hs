sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)

insert :: (Ord a) => a -> [a] -> [a]
insert v [] = [v]
insert v (x:xs)
    | v <= x    = v : x : xs         -- For reverse sort, use v >= x
    | otherwise = x : insert v xs

sortComplete :: (Ord a) => [a] -> [a]
sortComplete [] = []
sortComplete (x:xs) =
    insert x (sortComplete xs)
  where
    insert v [] = [v]
    insert v (x:xs)
        | v <= x    = v : x : xs        
        | otherwise = x : insert v xs
    

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

merge :: (Ord a) => [a] -> [a] -> [a]
merge l [] = l
merge [] l = l
merge (x:xs) (y:ys) 
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
-- mergesort l = 
--     let 
--         left = take' (length' l `div` 2) l
--         right = drop' (length' l `div` 2) l
--     in
--         merge (mergesort left) (mergesort right)
mergesort l =
    merge (mergesort left) (mergesort right)
  where
    middle = length' l `div` 2  
    left = take' middle l
    right = drop' middle l

isSorted :: (Ord a) => [a] -> Bool 
isSorted (x1:x2:xs) 
    | x1 <= x2  = isSorted xs
    | otherwise = False
isSorted _ = True