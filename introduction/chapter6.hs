map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

double :: (Num a) => a -> a
double x = 2 * x

halfList :: (Integral a) => [a] -> [a]
halfList l = map' (\x -> x `div` 2) l

quadruple :: (Num a) => a -> a
quadruple = double . double

(***) :: (Num a) => a -> a -> a
x *** y = 3*x*y

replace :: Char -> Char -> Char -> Char 
replace old new ltr
    | ltr == old = new
    | otherwise  = ltr 

calm :: [Char] -> [Char]
calm t = map' (replace '!' '.') t
-- calm [] = []
-- calm ('!':xs) = '.' : calm xs
-- calm (x:xs) = x : calm xs

clip :: (Ord a, Num a) => a -> a
clip x 
    | x > 10    = 10
    | x < 0     = 0
    | otherwise = x

clipList :: (Ord a, Num a) => [a] -> [a]
clipList l = map' clip l

clipList2 :: (Ord a, Num a) => [a] -> [a]
clipList2 l = map' (\x -> if x > 10 then 10 else (if x < 0 then 0 else x)) l

apply :: (Num a, Eq a) => (b -> b) -> a -> b -> b 
apply _ 0 acc = acc
apply f n acc = apply f (n-1) (f acc)

sort :: (Ord a) => [a] -> (a -> a -> Bool) -> [a]
sort [] _ = []
sort (x:xs) cmp = insert x (sort xs cmp) cmp

insert :: (Ord a) => a -> [a] -> (a -> a -> Bool) -> [a]
insert v [] _ = [v]
insert v (x:xs) cmp
    | cmp v x   = v : x : xs         -- v <= x
    | otherwise = x : insert v xs cmp

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) 
    | f x       = x : filter' f xs
    | otherwise = filter' f xs

all' :: (a -> Bool) -> [a] -> Bool 
all' _ [] = True 
all' f (x:xs) = f x && all' f xs

mapl :: (a -> b) -> [[a]] -> [[b]]
mapl _ [] = []
mapl f (x:xs) = map' f x : mapl f xs



    