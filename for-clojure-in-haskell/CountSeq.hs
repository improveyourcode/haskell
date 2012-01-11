countSeq [] = 0
countSeq (x:xs) = 1 + countSeq xs
