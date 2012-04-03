import Test.QuickCheck

multirember :: Eq a => a -> [a] -> [a]
multirember _ [] = []
multirember a (x:xs) | a == x = multirember a xs
                     | otherwise = x : multirember a xs
                                   
prop_remove_non_existent = forAll listOfNumbers $ \ns ->
                           forAll (numberNotIn ns) $ \n ->
                           multirember n ns == ns
                           
prop_remove_existent = forAll listOfNumbers $ \ns ->
                       forAll (listOfNumberNotIn ns) $ \nns ->
                       multirember (head nns) $ permutations
                                              
listOfNumbers :: Gen [Int]
listOfNumbers = vector 1

numberNotIn :: [Int] -> Gen Int
numberNotIn ns = arbitrary `suchThat` (\n -> not $ n `elem` ns)