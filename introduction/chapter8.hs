count' :: Num c => [(a,b)] -> c
-- count' [] = 0
-- count' (x:xs) = 1 + count' xs
count' = foldl (\acc _ -> 1 + acc) 0

replace' :: Eq a => a -> b -> [(a,b)] -> Maybe [(a,b)]
replace' _ _ [] = Nothing 
replace' k v ((ck,cv):xs) 
    | k == ck   = Just ((k, v) : xs)
    | otherwise = replace' k v xs >>= (\r -> Just ((ck,cv) : r))

combine' :: [a] -> [b] -> Maybe [(a,b)]
combine' [] [] = Just []
combine' (x:xs) (y:ys) = combine' xs ys >>= (\r -> Just((x,y):r))
combine' _ _ = Nothing

split' :: [(a,b)] -> ([a],[b])
split' = foldr (\(k,v) (kl, vl) -> (k:kl, v:vl)) ([],[])

create' :: Eq a => [(a,b)] -> [(a,b)]
create' = foldl (\acc x -> if exists' x acc then acc else x : acc) []
    where
        exists' _ [] = False
        exists' (k,v) ((ck,cv):xss) 
            | k == ck   = True
            | otherwise = exists' (k,v) xss

union' :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
union' l1 l2 = create' (l1 ++ l2)