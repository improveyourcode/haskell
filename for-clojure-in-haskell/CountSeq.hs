countSeq [] = 0
countSeq (x:xs) = 1 + countSeq xs

countSeq' xs = foldr (+) 0 xs
