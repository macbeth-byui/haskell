data Queue a = Queue {front :: [a], back :: [a]} deriving Show

enqueue :: a -> Queue a -> Queue a
enqueue v Queue {front = f, back = b} = rule Queue {front = f, back = v:b}

enqueueFront :: a -> Queue a -> Queue a
enqueueFront v Queue {front = f, back = b} = rule Queue {front = v:f, back = b}

dequeue :: Queue a -> Queue a
dequeue Queue {front = [], back = []} = Queue {front = [], back = []}
dequeue Queue {front = [], back = _} = Queue {front = [], back = []}
dequeue Queue {front = _, back = []} = Queue {front = [], back = []}
dequeue Queue {front = (_:fs), back = b} = rule Queue {front = fs, back = b}

dequeueBack :: Queue a -> Queue a
dequeueBack Queue {front = [], back = []} = Queue {front = [], back = []}
dequeueBack Queue {front = _, back = []} = Queue {front = [], back = []}
dequeueBack Queue {front = f, back = (_:bs)} = rule Queue {front = f, back = bs}

head :: Queue a -> Maybe a
head Queue {front = [], back = []} = Nothing 
head Queue {front = [], back = (b:_)} = Just b
head Queue {front = (f:_), back = _} = Just f

tail :: Queue a -> Maybe a
tail Queue {front = [], back = []} = Nothing 
tail Queue {front = (f:_), back = []} = Just f
tail Queue {front = _, back = (b:_)} = Just b

create :: Queue a
create = Queue {front = [], back = []}


rule :: Queue a -> Queue a
rule Queue {front = [], back = b} = 
    let (left, right) = splitAt (div (length b) 2) b
    in Queue {front = reverse right, back = left}
rule Queue {front = f, back = []} = 
    let (left, right) = splitAt (div (length f) 2) f
    in Queue {front = left, back = reverse right}
rule q = q