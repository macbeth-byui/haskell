import Data.Maybe ( fromMaybe )

data Seq a = Nil | Cons a (Seq a) deriving Show

-- Apply function to each element of the sequence to create a new sequence
mapSeq :: (a -> b) -> Seq a -> Seq b
mapSeq _ Nil = Nil
mapSeq f (Cons x xs) = Cons (f x) (mapSeq f xs)

-- Maybe support for the map lambda. 
maybeMapSeq :: (a -> Maybe b) -> Seq a -> Seq b
maybeMapSeq _ Nil = Nil
maybeMapSeq f (Cons x xs) =
    case f x of
        Just v  -> Cons v iter
        Nothing -> iter
    where
        iter = maybeMapSeq f xs

-- Apply function to each element of the sequence to determine if
-- it should be included in the new sequence.
filterSeq :: (a -> Bool) -> Seq a -> Seq a
filterSeq _ Nil = Nil
filterSeq f (Cons x xs) =
    let iter = filterSeq f xs in
        if f x then Cons x iter else iter

-- Maybe support for the filter lambda function.
maybeFilterSeq :: (a -> Maybe Bool) -> Seq a -> Seq a
maybeFilterSeq _ Nil = Nil
maybeFilterSeq f (Cons x xs) =
    case f x of
        Just v  -> if v then Cons x iter else iter
        Nothing -> iter
    where
        iter = maybeFilterSeq f xs

-- Apply a fold from left (front) to right (back) in the sequence starting with the
-- initial accumulator value.  The function expects the value first
-- and the accumulator second.
foldlSeq :: (a -> b -> b) -> b -> Seq a -> b
foldlSeq _ acc Nil = acc
foldlSeq f acc (Cons x xs) = foldlSeq f (f x acc) xs

-- Maybe support for the foldl lambda function.
maybeFoldlSeq :: (a -> b -> Maybe b) -> b -> Seq a -> b
maybeFoldlSeq _ acc Nil = acc
maybeFoldlSeq f acc (Cons x xs) =
    case f x acc of
        Just r  -> maybeFoldlSeq f r xs
        Nothing -> maybeFoldlSeq f acc xs

-- Simlilar to foldl except the sequence is processed from right (back) 
-- to left (front)
foldrSeq :: (a -> b -> b) -> b -> Seq a -> b
foldrSeq _ acc Nil = acc
foldrSeq f acc (Cons x xs) = f x (foldrSeq f acc xs)

-- Maybe support for the foldr lambda function.
maybeFoldrSeq :: (a -> b -> Maybe b) -> b -> Seq a -> b
maybeFoldrSeq _ acc Nil = acc
maybeFoldrSeq f acc (Cons x xs) =
    -- fromMaybe will use the value if Just otherwise acc if Nothing
    fromMaybe acc (f x (maybeFoldrSeq f acc xs))


-- TODO: Create unfoldrSeq and maybeUnfoldrSeq

-- Generate a sequence with length (first parameter), initial
-- value (second parameter) and a function that will generate
-- the next value from the current value.
unfoldlSeq :: (Num a, Eq a) => a -> b -> (b -> b) -> Seq b
unfoldlSeq 0 _ _ = Nil
unfoldlSeq n curr f = Cons curr (unfoldlSeq (n-1) (f curr) f)

-- Maybe support for the unfold lambda function.  Instead of providing
-- a length, the unfold will finish when the lambda returns Nothing.
maybeUnfoldlSeq :: a -> (a -> Maybe a) -> Seq a
maybeUnfoldlSeq curr f = 
    case f curr of
        Just r  -> Cons curr (maybeUnfoldlSeq r f)
        Nothing -> Cons curr Nil

-- Determine length of the sequence
lengthSeq :: Num b => Seq a -> b
lengthSeq Nil = 0
lengthSeq (Cons _ xs) = 1 + lengthSeq xs

-- Prepend to the front of the sequence
prependSeq :: a -> Seq a -> Seq a
prependSeq = Cons

-- Append to the back of the sequence
appendSeq :: a -> Seq a -> Seq a
appendSeq v Nil = Cons v Nil
appendSeq v (Cons x xs) = Cons x (appendSeq v xs)

-- Pop the first value off of the sequence.  Maybe is used
-- to indicate if pop of empty sequence was attempted.
popSeq :: Seq a -> Maybe (Seq a)
popSeq Nil = Nothing
popSeq (Cons _ xs) = Just xs

-- Push a new value to the front of the sequence.  This is the
-- same as prepend.
pushSeq :: a -> Seq a -> Seq a
pushSeq = prependSeq

-- Pop the last value from the sequence.  Maybe is used 
-- to indicate if popBack of empty sequence was attempted.
popBackSeq :: Seq a -> Maybe (Seq a)
popBackSeq Nil = Nothing
popBackSeq (Cons _ Nil) = Just Nil
popBackSeq (Cons x xs) = 
    do
        r <- popBackSeq xs
        Just (Cons x r)

-- Push a new value to the back of the sequence.  This is the
-- same as append.
pushBackSeq :: a -> Seq a -> Seq a
pushBackSeq = appendSeq

-- Return the first value of the sequence.  Maybe is used
-- to indicate if the sequence is empty.
headSeq :: Seq a -> Maybe a
headSeq Nil = Nothing
headSeq (Cons x _) = Just x

-- Return the last value of the sequence.  Maybe is used 
-- to indicate if the sequence is empty.
tailSeq :: Seq a -> Maybe a
tailSeq Nil = Nothing
tailSeq (Cons x Nil) = Just x
tailSeq (Cons x xs) = tailSeq xs

-- Create a new sequence with the values reversed.
reverseSeq :: Seq a -> Seq a
reverseSeq Nil = Nil
reverseSeq (Cons x xs) = Cons x (reverseSeq xs)

-- Insert a new value (first parameter) at the index (second parameter)
-- into the sequence.  Maybe is used in case of invalid index.  The
-- index is 0 based.
insertAtSeq :: (Num b, Ord b) => a -> b -> Seq a -> Maybe (Seq a)
insertAtSeq v 0 s = Just (Cons v s)
insertAtSeq _ _ Nil = Nothing
insertAtSeq v n (Cons x xs) = 
    do
        r <- insertAtSeq v (n-1) xs
        Just (Cons x r)

-- Remove a value at the index (first parameter) from the sequence.  Maybe
-- is used in case of invalid index.  The index is 0 based.
removeAtSeq :: (Num a, Ord a) => a -> Seq b -> Maybe (Seq b)
removeAtSeq 0 (Cons _ xs) = Just xs
removeAtSeq _ Nil = Nothing
removeAtSeq n (Cons x xs) = 
    do
        r <- removeAtSeq (n-1) xs
        Just (Cons x r)

-- Extract a specified number of values (first parameter) from the 
-- sequence into a new sequence.  Maybe is used in case an invalid number
-- values is provided.
takeSeq :: (Num a, Eq a) => a -> Seq b -> Maybe (Seq b)
takeSeq 0 _ = Just Nil
takeSeq _ Nil = Nothing
takeSeq n (Cons x xs) = 
    do
        r <- takeSeq (n-1) xs
        Just (Cons x r)

-- Remove a specified number of values (first parameter) from
-- the sequence leaving only the remaining sequence values.  Maybe is used
-- in case an invalid number of values is provided.
dropSeq :: (Num a, Eq a) => a -> Seq b -> Maybe (Seq b)
dropSeq 0 s = Just s
dropSeq _ Nil = Nothing
dropSeq n (Cons _ xs) = dropSeq (n-1) xs

-- Indicate if the sequence is empty
isEmptySeq :: Seq a -> Bool
isEmptySeq s = lengthSeq s == 0

-- Is an element in a sequence
elemSeq :: Eq a => a -> Seq a -> Bool
elemSeq _ Nil = False
elemSeq v (Cons x xs) = (v == x) || elemSeq v xs
