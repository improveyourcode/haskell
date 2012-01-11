anagramFinder :: [String] -> [[String]]
anagramFinder xs = anagramFinder' xs []

anagramFinder' :: [String] -> [[String]] -> [[String]]
anagramFinder' [] _ = [[]]
anagramFinder' (x:xs) anagramsMap = let newAnagramsMap = updateAnagrams x anagramsMap
                                    in anagramFinder' xs newAnagramsMap

updateAnagrams :: String -> [[String]] -> [[String]]
updateAnagrams anagram [] = [(sort anagram,[])]
updateAnagrams anagram ((sortedAnagram, anagrams):rest) | (sort anagram) == sortedAnagram = ((sortedAnagram, addIfNotPresent anagram anagrams):rest)
                                                        | otherwise = updateAnagrams anagram rest 
                                                                      
