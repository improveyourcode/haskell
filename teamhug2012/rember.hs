import Test.QuickCheck

rember :: Eq a => a -> [a] -> [a]
rember _ [] = []
rember a (x:xs) | a == x = xs
                | otherwise = x:(rember a xs)
                              
prop_remove_doesnt_exist = forAll lists $ \xs ->
                           forAll (elementsNotIn xs) $ \x -> 
                           rember x xs == xs
                           
prop_remove_existing = forAll lists $ \xs ->
                       forAll (elementsIn xs) $ \x ->
                       (count x xs) - 1 == count x (rember x xs) 

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count a (x:xs) | a == x = 1 + (count a xs)
               | otherwise = count a xs
                           
lists :: Gen [String]
lists = vector 1

elementsNotIn :: [String] -> Gen String
elementsNotIn xs = anyElement `suchThat` isNotInXs
                   where anyElement = arbitrary
                         isNotInXs = \x -> not $ x `elem` xs 
                         
elementsIn :: [String] -> Gen String
elementsIn xs = fmap (\i -> xs !! i) $ choose(0, (length xs) - 1)
