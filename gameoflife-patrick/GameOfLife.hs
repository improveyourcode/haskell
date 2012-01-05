import Data.Map (Map)
import Data.List
import qualified Data.Map as Map

type Grid = Map (Int, Int) Bool 

-- Game of life, by Patrick Robotham

toInt :: Bool -> Int
toInt True  = 1
toInt False = 0

getCell0 :: (Int,Int) -> Grid -> Bool
getCell0 = Map.findWithDefault False

getCell = flip getCell0

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = [(pred x, pred y), (x, pred y), (succ x, pred y),
                    (pred x, y)     , (x, y)     , (succ x, y),
                    (pred x, succ y), (x, succ y), (succ x, succ y)]
                   
rules :: Bool -> Int -> Bool
-- takes number of alive neigbours and decides whether cell lives or dies
rules False 3 = True
rules True  3 = True
rules True  4 = True
rules _  _ = False

aliveNeighbours :: (Int,Int) -> Grid -> Int
aliveNeighbours c g = sum $ map (toInt. (getCell g)) (neighbours c)

lives :: (Int, Int) -> Grid -> Bool 
lives c g = rules (getCell g c) (aliveNeighbours c g)

nextState :: Grid -> Grid
nextState g = contract $ Map.mapWithKey (\k v ->  lives k g) $ expand g

expand :: Grid -> Grid
-- adds all cell's neighbours to the grid.
expand g = 
  let cells  = (fst . unzip . Map.toList) g
      neighs = nub.(concatMap neighbours) $ cells
  in Map.fromList $ zip neighs (map (getCell g) neighs)

contract :: Grid -> Grid
-- removes superfluous cells from grid
contract g = 
  let superfluous :: (Int,Int) -> Bool -> Bool
      superfluous c _ = or $ map (getCell g) (neighbours c) 
  in Map.filterWithKey superfluous g 

showCell :: Bool -> String
showCell True = "@" 
showCell False = " "

showRow ::  Grid -> Int ->  String
-- shows the first 80 cells in a row.
showRow g r = (concatMap (showCell.(getCell g)) $ zip (repeat r) [0..79]) ++ "\n" 


showGrid :: Grid -> String
-- shows an 80*80 view of the grid.
showGrid g = (take 80 $ repeat '=') ++ "\n" ++
             (concatMap (showRow g) [0..22]) ++ 
              (take 80 $ repeat '=') ++ "\n"

life :: Grid -> IO Grid
life g = 
  do
    putStr (showGrid g)
    getLine
    life $ nextState g

-- testing

build :: [(Int, Int)] -> Grid
build lst = Map.fromList $ zip lst (repeat True)

testgrid :: Grid
testgrid = build (neighbours (11,40)) 

brick :: Grid
brick = build [(0,0) , (0,1) , (1,0) , (1,1)]

flipper :: Grid
flipper = build [(0,0), (1,0), (2,0)] 

dead :: Grid
dead = build [(0,0),(0,1)]

tencellrow :: Grid
tencellrow = build [(11,30),(11,31),(11,32),(11,33),(11,34),(11,35),
                    (11,36),(11,37),(11,38),(11,39),(11,39)]
