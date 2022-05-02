import Data.Maybe ( mapMaybe )

mapl :: (a -> b) -> [[a]] -> [[b]]
-- mapl _ [] = []
-- mapl f (x:xs) = map f x : mapl f xs
-- mapl f l = map (map f) l  -- (map f) is a function that takes a list as input and returns a list
-- mapl f = map (map f)      -- returns a function that takes a list of lists and returns a lists of lists
mapl = map . map          -- Use composition - function that takes a function that returns a function that takes a list of lists and returns a lists of lists

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' v (x:xs) = (v == x) || elem' v xs

elemAll' :: Eq a => a -> [[a]] -> Bool
-- elemAll' v l = not (elem' False (map (elem' v) l))
elemAll' v = not . elem' False . map (elem' v)
-- Note... A fold would be better here 

mapll :: (a -> b) -> [[[a]]] -> [[[b]]]
-- mapll _ [] = []
-- mapll f (x:xs) = mapl f x : mapll f xs
-- mapll f l = map (mapl f) l
-- mapll f = map (mapl f)
-- mapll = map . mapl
mapll = map . map . map

take' :: (Num a, Eq a) => a -> [b] -> Maybe [b]
take' 0 _ = Just []
take' _ [] = Nothing
take' n (x:xs) = take' (n-1) xs >>= (\r -> Just (x:r))

truncateList :: (Num a, Eq a) => a -> [[b]] -> [[b]]
truncateList = mapMaybe . take'

firsts :: Num a => [[a]] -> [a]
firsts = map (\l -> case l of 
    [] -> 0
    (x:_) -> x)

(+>) :: Num a => a -> [[a]] -> [[a]]
(+>) n = map (n:) 