import Data.List as L
import Data.Map as M

type Anagrams = [String]

anagramFinder :: [String] -> [[String]]
anagramFinder xs = L.filter (\l -> (length l) > 1) $ M.elems $ foldr (\word anagramsMap -> addOrUpdate word anagramsMap) M.empty  xs

addOrUpdate :: String -> Map String Anagrams -> Map String Anagrams
addOrUpdate word anagramsMap = case M.lookup sortedWord anagramsMap of
                                 Just anagrams -> M.insertWith (++) sortedWord [word] anagramsMap
                                 Nothing -> M.insert sortedWord [word] anagramsMap
                               where sortedWord = sort word
