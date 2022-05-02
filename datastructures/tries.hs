import Data.Map

data Trie = Nil | Node (Map Char Trie) deriving Show

add :: [Char] -> Trie -> Trie
add x Nil = add x (Node empty)
add [] (Node m)
    | member '\0' m = Node m
    | otherwise     = Node (insert '\0' Nil m)
add (x:xs) (Node m) =
    let target = findWithDefault (Node empty) x m
    in Node (insert x (add xs target) m)

search :: [Char] -> Trie -> Bool 
search _ Nil = False 
search [] (Node m) = member '\0' m 
search (x:xs) (Node m) = 
    case Data.Map.lookup x m of
        Just r  -> search xs r
        Nothing -> False
 
count :: Trie -> Int 
count Nil = 0
count (Node m) 
    | member '\0' m = 1 + childCount
    | otherwise     = childCount
    where
        childCount = Data.Map.foldr (\ v acc -> acc + count v) 0 m 
