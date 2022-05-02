import Debug.Trace ( trace )
makePrimes :: [Integer] -> [Integer]
makePrimes (x:xs) =
    x : makePrimes (filter' (\ n -> rem n x /= 0) xs)

-- The trace is useful for quick debugging
filter' :: Show a => (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) 
    | f x       = trace ("a"++show x) (x : filter' f xs)
    | otherwise = trace ("b"++show x) (filter' f xs)

power2 :: Integer -> [Integer]
power2 x = x : power2 (x*2)

repeatList :: [a] -> [a]
repeatList l = l ++ repeatList l

fibList :: Integer -> Integer -> [Integer]
fibList x y = x : fibList y (x+y)

-- Different approach where the 0 and 1 are built in
fibList2 :: [Integer]
fibList2 = fibList2' 0 1
    where
        fibList2' x y = x : fibList2' y (x+y)



unleave :: [a] -> ([a],[a])
unleave [] = ([],[])
unleave [x] = ([x],[])
unleave (x1:x2:xs) = 
    let (left,right) = unleave xs
    in (x1:left, x2:right)
    