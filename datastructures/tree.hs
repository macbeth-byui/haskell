data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree v Nil = Node v Nil Nil
insertTree v n@(Node nv l r)
    | v < nv    = Node nv (insertTree v l) r
    | v > nv    = Node nv l (insertTree v r)
    | otherwise = n

searchTree :: Ord a => a -> Tree a -> Bool
searchTree v Nil = False
searchTree v (Node nv l r)
    | v == nv   = True
    | v < nv    = searchTree v l
    | otherwise = searchTree v r

isEmptyTree :: Tree a -> Bool
isEmptyTree Nil = True
isEmptyTree _   = False

getRemoveMaxTree :: Tree a -> Maybe (Tree a, a)
getRemoveMaxTree Nil = Nothing
getRemoveMaxTree (Node v _ Nil) = Just (Nil, v)
getRemoveMaxTree (Node v l r) = do
    (newRight, maxRight) <- getRemoveMaxTree r
    Just (Node v l newRight, maxRight)

removeTree :: Ord a => a -> Tree a -> Tree a
removeTree v Nil = Nil
removeTree v (Node nv Nil r)
    | v == nv   = r
    | otherwise = Node nv Nil (removeTree v r)
removeTree v (Node nv l r)
    | v == nv   = do
        case getRemoveMaxTree l of
            Just (newLeft, maxLeft) -> Node maxLeft newLeft r
            Nothing  -> l
    | v < nv    = Node nv (removeTree v l) r
    | otherwise = Node nv l (removeTree v r)

-- t = foldl (\acc v -> insertTree v acc) Nil [4,2,6,1,3,5,7]
-- foldl (\acc v -> removeTree v acc) t [1,2,3,4,5,6,7] 

data RBTColor = Red | Black deriving Show
data RBT a = RBTNil | RBTNode a RBTColor (RBT a) (RBT a) deriving Show

insertRBT :: Ord a => a -> RBT a -> RBT a
insertRBT v rbt =
    let
        (RBTNode rv _ l r) = insertRBT_ v rbt
        insertRBT_ v RBTNil = RBTNode v Red RBTNil RBTNil
        insertRBT_ v n@(RBTNode nv c l r)
            | v < nv    = balanceRBT (RBTNode nv c (insertRBT_ v l) r)
            | v > nv    = balanceRBT (RBTNode nv c l (insertRBT_ v r))
            | otherwise = n
    in RBTNode rv Black l r

balanceRBT :: RBT a -> RBT a
balanceRBT (RBTNode z Black (RBTNode x Red a (RBTNode y Red b c)) d) =
    RBTNode y Red (RBTNode x Black a b) (RBTNode z Black c d)
balanceRBT (RBTNode x Black a (RBTNode y Red b (RBTNode z Red c d))) =
    RBTNode y Red (RBTNode x Black a b) (RBTNode z Black c d)
balanceRBT (RBTNode x Black a (RBTNode z Red (RBTNode y Red b c) d)) =
    RBTNode y Red (RBTNode x Black a b) (RBTNode z Black c d)
balanceRBT (RBTNode z black (RBTNode y Red (RBTNode x Red a b) c) d) =
    RBTNode y Red (RBTNode x Black a b) (RBTNode z Black c d)
balanceRBT n = n

-- t = foldl (\acc v -> insertRBT v acc) RBTNil [1,2,3,4,5,6,7]