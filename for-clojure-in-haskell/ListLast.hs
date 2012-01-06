listLast [] = error "Cannot determine last element for an empty list"
listLast [x] = x
listLast (x:xs) = listLast xs
