data Color = Red | Green | Blue | Yellow deriving Show
data Maybe' a = Just' a | Nothing' deriving Show

data Rect a = Rectangle a a | Square a deriving Show

area :: Num a => Rect a -> a
area (Rectangle x y) = x * y
area (Square x) = x * x

rotate :: Ord a => Rect a -> Rect a
rotate (Rectangle w h)
    | w > h     = Rectangle h w
    | otherwise = Rectangle w h
rotate r = r

sort :: [a] -> (a -> a -> Bool) -> [a]
sort [] _ = []
sort (x:xs) cmp = insert x (sort xs cmp) cmp

insert :: a -> [a] -> (a -> a -> Bool) -> [a]
insert v [] _ = [v]
insert v (x:xs) cmp
    | cmp v x   = v : x : xs
    | otherwise = x : insert v xs cmp

width :: Rect a -> a
width (Rectangle w _) = w
width (Square s) = s

compareRect :: Ord a => Rect a -> Rect a -> Bool
compareRect r1 r2 = width r1 < width r2

sortRect :: Ord a => [Rect a] -> [Rect a]
sortRect l = sort (map rotate l) compareRect

-- *Main> sortRect [Rectangle 3 2, Rectangle 4 1, Square 1, Rectangle 10 100]
-- [Square 1,Rectangle 1 4,Rectangle 2 3,Rectangle 10 100]

data Sequence a = Nil | Cons a (Sequence a) deriving Show

seqTake :: (Num a, Eq a) => a -> Sequence b -> Sequence b
seqTake 0 s = Nil
seqTake _ Nil = Nil
seqTake n (Cons x xs) = Cons x (seqTake (n-1) xs)

seqDrop :: (Num a, Eq a) => a -> Sequence b -> Sequence b
seqDrop 0 s = s
seqDrop _ Nil = Nil
seqDrop n (Cons x xs) = seqDrop (n-1) xs

seqMap :: (a -> b) -> Sequence a -> Sequence b
seqMap _ Nil = Nil
seqMap l (Cons x xs) = Cons (l x) (seqMap l xs)

data Expr a = Num a
    | Add (Expr a) (Expr a)
    | Subtract (Expr a) (Expr a)
    | Multiply (Expr a) (Expr a)
    | Divide (Expr a) (Expr a)
    | Power (Expr a) (Expr a) deriving Show

evaluate :: (Integral a, Eq a) => Expr a -> Maybe a
evaluate e = case e of
    Num x -> Just x
    Add x y -> perform x y (+)
    Subtract x y -> perform x y (-)
    Multiply x y -> perform x y (*)
    Divide x (Num 0) -> Nothing
    Divide x y -> perform x y div
    Power x y -> perform x y (^)
    where
        perform x' y' op = do
            xx' <- evaluate x'
            yy' <- evaluate y'
            Just (op xx' yy')
        -- perform x y op = evaluate x >>= (\xx -> evaluate y >>= (Just . op xx))
        
