import System.Cmd
import Control.Concurrent

type Cell = ((Int,Int),Int)

gameOfLife file = do
                  content <- readFile file
                  let (len,generation) = parseContent content
                  printGenerationsFrom len generation

parseContent :: String -> (Int,[Cell])
parseContent content = let rows = lines content
                           len = length rows
                           values = map (\x -> if x == '.' then 0 else 1) $ concat rows
                           positions = [(x,y) | x <- [1..len], y <- [1..len]]
                       in (len,zip positions values)

printGenerationsFrom len generation = do
                                      system "clear"
                                      printGeneration len generation
                                      threadDelay 100000
                                      let newGeneration = nextGeneration len generation
                                      printGenerationsFrom len newGeneration
                          
printGeneration len [] = return ()
printGeneration len generation = do
                                   let (row,rows) = splitAt len generation
                                   mapM_ (\(_,v) -> if v == 0 then putChar '.' else putChar '@') row
                                   putStrLn ""
                                   printGeneration len rows

nextGeneration :: Int -> [Cell] -> [Cell]
nextGeneration len generation = map (applyRules len generation) generation

applyRules :: Int -> [Cell] -> Cell -> Cell
applyRules len generation cell@((x,y),v) = let numberOfAliveNeighbors = length $ filter (\(_,c) -> c == 1) $ neighbors len generation cell
                                           in applyGameRules cell numberOfAliveNeighbors

applyGameRules (position,v) aliveNeighbors | v == 0 && aliveNeighbors == 3 = (position,1)
                                           | v == 1 && aliveNeighbors `elem` [2,3] = (position,1)
                                           | otherwise = (position,0)

neighbors len generation ((x,y),_) = [((a,b),v) | ((a,b),v) <- generation, (a,b) /= (x,y), (abs $ x - a) <= 1, (abs $ y - b) <= 1, a >= 1, a <= len, b >= 1, b <= len]
