safeDivide :: (Integral a) => a -> a -> Maybe a
safeDivide x 0 = Nothing 
safeDivide x y = Just (x `div` y)

safeTake :: (Num a, Ord a) => a -> [b] -> Maybe [b]
safeTake n l
    | n <= 0 || n > length' l = Nothing 
    | otherwise               = Just (take' n l)

safeDrop :: (Num a, Ord a) => a -> [b] -> Maybe [b]
safeDrop n l
    | n <= 0 || n > length' l = Nothing 
    | otherwise               = Just (drop' n l)

take' :: (Num a, Eq a) => a -> [b] -> [b]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: (Num a, Eq a) => a -> [b] -> [b]
drop' 0 _ = []
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

length' :: Num b => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
    case f x of
        Nothing -> rs
        Just r  -> r : rs
    where
        rs = mapMaybe f xs
        
safeDivide2 :: (Integral a) => a -> a -> Either String a  
safeDivide2 x 0 = Left "Division by 0" 
safeDivide2 x y = Right (x `div` y)

smallest :: (Num a, Ord a) => [a] -> Maybe a
smallest [] = Nothing 
smallest (x:xs) = 
    case smallest xs of
        Nothing -> if x > 0 then Just x else Nothing
        Just a  -> if x < a && x > 0 then Just x else Just a

smallest0 :: (Num a, Ord a) => [a] -> a
smallest0 l =
    case smallest l of
        Nothing -> 0
        Just a  -> a
        
sqrtMaybe :: (Num a, Ord a) => a -> Maybe a
sqrtMaybe n
    | n < 0     = Nothing 
    | otherwise = Just (iter 1 n) 
        where
            iter x n = if x * x > n then x - 1 else iter (x+1) n
            
mapMaybe2 :: (a -> Maybe b) -> [a] -> b -> [b]
mapMaybe2 _ [] _ = []
mapMaybe2 f (x:xs) d = 
    case f x of
        Nothing -> d : rs
        Just r  -> r : rs
    where
        rs = mapMaybe2 f xs d

-- mapMaybe2 (\x -> if x > 0 then Just (2*x) else Nothing) [1,4,9,-16,25] (-100)

splitEither :: (a -> Either b c) -> [a] -> ([b],[c])
splitEither _ [] = ([],[])
splitEither f (x:xs) = 
    case f x of
        Left r -> (r : left, right)
        Right r -> (left, r : right)
    where
        (left, right) = splitEither f xs