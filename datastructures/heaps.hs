import Control.Monad (foldM)
data Heap a = Nil | Node Int a (Heap a) (Heap a) deriving Show

rank :: Heap a -> Int
rank Nil = 0
rank (Node rank _ _ _) = rank

make :: a -> Heap a -> Heap a -> Heap a
make v l r 
    | rank l >= rank r = Node (1 + rank r) v l r
    | otherwise        = Node (1 + rank l) v r l

merge :: Ord a => Heap a -> Heap a -> Heap a
merge heap Nil = heap
merge Nil heap = heap
merge heap1@(Node rank1 v1 l1 r1) heap2@(Node rank2 v2 l2 r2)
    | v1 <= v2  = make v1 l1 (merge r1 heap2)
    | otherwise = make v2 l2 (merge r2 heap1)

insert :: Ord a => a -> Heap a -> Heap a
insert v = merge (make v Nil Nil)

removeMin :: Ord a => Heap a -> Maybe (Heap a)
removeMin Nil = Nothing
removeMin (Node _ _ l r) = Just (merge l r)

-- h = foldl (\ acc v -> insert v acc) Nil [5,10,15,20,13,16,11,6,0]
-- h2 = foldM (\ acc v -> removeMin acc) h [5,10,15,20,13,16,11,6,0]