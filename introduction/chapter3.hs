import Data.Char

not_ :: Bool -> Bool
not_ True = False
not_ False = True

sumMatch :: (Num a, Eq a) => a -> a
sumMatch 1 = 1
sumMatch n = n + sumMatch (n-1)

power :: (Num a, Num b, Eq b) => a -> b -> a
power _ 0 = 1
power x n = x * power x (n-1)

kind :: (Num a) => Char -> a
kind c | isAsciiLower c = 0
       | isAsciiUpper c = 1
       | otherwise      = 2