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

-- Returns the score of two Chars
score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y = if x == y then scoreMatch else scoreMismatch

-- Returns the score of the optimal alignment of two strings
similarityScore :: String -> String -> Int
similarityScore [] _ = 0
similarityScore _ [] = 0
similarityScore (s1 : s1s) (s2 : s2s) =
  maximum
    [ similarityScore s1s s2s + score s1 s2,
      similarityScore s1s (s2 : s2s) + score s1 '-',
      similarityScore (s1 : s1s) s2s + score '-' s2
    ]

-- Attaches h1 and h2 to the list in fst and snd respectively
-- in a list of pairs
attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1 : xs, h2 : ys) | (xs, ys) <- aList]

-- Finds the list of maximas of a list, using a provided function to find the order
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn = foldl (maxiB valueFcn) []
  where
    maxiB _ [] x = [x]
    maxiB f acc x
      | newValue > currentMax = [x]
      | newValue == currentMax = x : acc
      | otherwise = acc
      where
        newValue = f x
        currentMax = f (head acc)

-- Returns a list of all optimal alignments between two strings
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] _ = []
optAlignments _ [] = []
optAlignments (s1 : s1s) (s2 : s2s) =
  maximaBy
    pairScore
    (attachHeads s1 s2 $ optAlignments s1s s2s) -- We don't insert any space here and find alignment for rest of the Strings
    ++ attachHeads s1 '-' (optAlignments s1s (s2 : s2s)) -- We insert space instead first char of second string s2 and try to find the maximum for rest of s1 and the whole of s2
    ++ attachHeads '-' s2 (optAlignments (s1 : s1s) s2s) -- Line above but we insert space instead of s1 instead
  where
    -- Recursively finds the score of an alignment by comparing the two strings
    -- one char at a time
    pairScore :: AlignmentType -> Int
    pairScore ([], _) = 0
    pairScore (_, []) = 0
    pairScore (x : xs, y : ys) = pairScore (xs, ys) + score x y