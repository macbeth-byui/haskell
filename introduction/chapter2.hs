mult10 :: Num a => a -> a
mult10 n = n * 10

bothNonZero :: (Num a, Eq a) => a -> a -> Bool
bothNonZero x y = x /= 0 && y /= 0

sum_ :: (Eq a, Num a) => a -> a
sum_ 0 = 0
sum_ n = n + sum_ (n-1)

power :: (Num a, Num b, Eq b) => a -> b -> a
power x 0 = 1
power x n = x * power x (n-1)

isConsonant :: Char -> Bool
isConsonant c = not (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u')

factorial :: (Num a, Ord a) => a -> a
factorial n | n == 0    = 1
            | n > 0     = n * factorial (n-1)
            | otherwise = n * factorial (n+1) 
