-- EDAN40 A2 String Alignment by Emil Eriksson (em5184er-s) && Lukas Elmlund (lu0804el-s)


-- Hardcoded values
scoreMatch :: Int
scoreMismatch :: Int
scoreSpace :: Int
string1 :: [Char]
string2 :: [Char]

scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2
string1 = "writers"
string2 = "vintner"

-- Types
type AlignmentType = (String, String)

-- Functions
similarityScore :: String -> String -> Int
similarityScore [] _ = 0
similarityScore _ [] = 0
similarityScore (s1:s1s) (s2:s2s)
    = maximum [similarityScore s1s s2s + score s1 s2,
            similarityScore s1s (s2:s2s) + score s1 '-',
            similarityScore (s1:s1s) s2s + score '-' s2]
    where score x '-'  = scoreSpace
          score '-' y = scoreSpace
          score x y = if x == y then scoreMatch else scoreMismatch


-- Attaches h1 and h2 to the list in fst and snd respectively
-- in a list of pairs
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn = foldl (maxiB valueFcn) []
    where
        maxiB _ [] x = [x]
        maxiB f acc x
            | newValue > currentMax = [x]
            | newValue == currentMax = x:acc
            | otherwise = acc
                where newValue = f x
                      currentMax = f (head acc)