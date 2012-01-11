import Data.List

anagramFinder [] = []
anagramFinder xs = filter (\l -> (length l) > 1) $ map snd $  anagramFinder' xs []

anagramFinder' [] anagrams = anagrams
anagramFinder' (x:xs) anagrams = let anagrams' = addOrUpdate x anagrams anagrams
                                 in anagramFinder' xs anagrams'

addOrUpdate anAnagram [] anagramsAcc = ((sort anAnagram),[anAnagram]):anagramsAcc
addOrUpdate anAnagram ((anagram, anagrams):rest) anagramsAcc
    | ((sort anAnagram) == anagram) = if not $ anAnagram `elem` anagrams
                                        then (anagram, (anAnagram:anagrams)):rest
                                        else ((anagram, anagrams):rest)
    | otherwise = addOrUpdate anAnagram rest anagramsAcc
