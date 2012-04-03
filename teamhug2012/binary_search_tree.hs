import Test.QuickCheck

data Tree a = Node a (Tree a) (Tree a) | Empty deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert value Empty = Node value Empty Empty
insert value (Node n leftBranch rightBranch) | value < n = Node n (insert value leftBranch) rightBranch
                                             | otherwise = Node n leftBranch (insert value rightBranch)
                                                           
prop_insert_preserves_order = forAll intBinarySearchTrees $ \tree ->
                              forAll ints $ \int ->
                              (isOrderedTree $ insert int tree) == True
                             
intBinarySearchTrees :: Gen (Tree Int)
intBinarySearchTrees = fmap makeTree orderedIntList

makeTree :: [Int] -> Tree Int
makeTree [] = Empty
makeTree [x] = Node x Empty Empty
makeTree xs = Node middleElement (makeTree $ takeUpTo middleIndex) (makeTree $ takeFrom middleIndex)
              where middleIndex = (length xs - 1) `div` 2
                    middleElement = xs !! middleIndex
                    takeUpTo 0 = []
                    takeUpTo n = take n xs
                    takeFrom n = drop (n + 1) xs

orderedIntList :: Gen [Int]
orderedIntList = intList `suchThat` isOrdered

intList :: Gen [Int]
intList = resize 31 $ arbitrary

isOrdered :: [Int] -> Bool
isOrdered [] = True
isOrdered [x] = True
isOrdered (x:y:rest) = x < y && isOrdered (y:rest)

ints :: Gen Int
ints = arbitrary

isOrderedTree :: Ord a => Tree a -> Bool
isOrderedTree Empty = True
isOrderedTree (Node n leftBranch rightBranch) = is (<n) leftBranch && is (n<) rightBranch && isOrderedTree leftBranch && isOrderedTree rightBranch

is :: (a -> Bool) -> Tree a -> Bool
is _ Empty = True 
is comp (Node n _ _) = comp n