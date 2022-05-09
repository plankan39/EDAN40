-- EDAN40 A2 String Alignment by Emil Eriksson (em5184er-s) && Lukas Elmlund (lu0804el-s)


-- Hardcoded values
scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2
string1 = "writers"
string2 = "vintner"

similarityScore :: String -> String -> Int
similarityScore s1 s2 = 0

-- Attaches h1 and h2 to the list in fst and snd respectively
-- in a list of pairs
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList] 