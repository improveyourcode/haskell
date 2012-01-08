listLast [] = error "Cannot determine last element for an empty list"
listLast [x] = x
listLast (x:xs) = listLast xs

listLast' :: [a] -> Maybe a
listLast' [] = Nothing
listLast' [x] = Just x
listLast' (x:xs) = listLast' xs

-- Idempotency property (sort of as listLast return an element and not a list)
prop_listLast xs = listLast [listLast xs] == listLast xs

-- Repeating listLast' to the list of the result of applying listLast' to a list should equal to applying listLast' to the same list
prop_listLast' xs = ((listLast' xs) >>= (\x -> listLast' [x])) == listLast' xs

-- The initial elements concatenated with the last element should equal the entire list
prop_concatLast' xs = ((listLast' xs) >>= (\x -> (Just ((init xs) ++ [x])) )) == Just xs
