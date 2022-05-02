data RALTree
    a = Nil |
    Node Integer (RALTree a) (RALTree a) |
    Leaf a
    deriving Show

prepend :: a -> [RALTree a] -> [RALTree a]
prepend v = merge (Leaf v)

count :: RALTree a -> Integer
count Nil = 0
count (Node c _ _) = c
count (Leaf _) = 1

merge :: RALTree a -> [RALTree a] -> [RALTree a]
merge nrt [] = [nrt]
merge nrt (Nil:rts) = nrt:rts
merge nrt (rt:rts) =
    let mt = Node (2 * count nrt) nrt rt
    in Nil:merge mt rts

-- t = foldl (\ acc v -> prepend v acc) [] [1,2,3,4,5,6,7,8]

lookup' :: Integer -> [RALTree a] -> Maybe a
lookup' i [] = Nothing
lookup' i (Nil:rts) = lookup' i rts
lookup' i (rt:rts)
    | i < 0         = Nothing
    | i >= count rt = lookup' (i - count rt) rts
    | otherwise     = lookupInTree i rt

lookupInTree :: Integer -> RALTree a -> Maybe a
lookupInTree i Nil = Nothing
lookupInTree _ (Leaf v) = Just v
lookupInTree i (Node c l r)
    | i < div c 2 = lookupInTree i l
    | otherwise   = lookupInTree (i - div (count r) 2) r


