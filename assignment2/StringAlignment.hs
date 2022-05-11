import Data.List (intersperse)

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

-- Recursively finds the score of an alignment by comparing the two strings
-- one char at a time
pairScore :: AlignmentType -> Int
pairScore ([], _) = 0
pairScore (_, []) = 0
pairScore (x : xs, y : ys) = pairScore (xs, ys) + score x y

-- Returns the score of the optimal alignment of two strings
similarityScore' :: String -> String -> Int
similarityScore' [] _ = 0
similarityScore' _ [] = 0
similarityScore' (s1 : s1s) (s2 : s2s) =
  maximum
    [ similarityScore' s1s s2s + score s1 s2,
      similarityScore' s1s (s2 : s2s) + score s1 '-',
      similarityScore' (s1 : s1s) s2s + score '-' s2
    ]

similarityScore :: String -> String -> Int
similarityScore [] _ = 0
similarityScore _ [] = 0
similarityScore s1 s2 = simSco (length s1) (length s2) -- Lengths are indexes, we go from the back
  where
    simSco i j = simTable !! i !! j -- We get all data from the table
    simTable = [[simEntry i j | j <- [0 ..]] | i <- [0 ..]] -- Generate table, where every entry is simEntry called on index

    -- Entry of our simTable, takes indexes and returns score of that entry
    simEntry :: Int -> Int -> Int
    simEntry 0 _ = 0 -- We compare an empty list to some string in the table
    simEntry _ 0 = 0
    simEntry i j
      -- These are where the scoring is done, we recursively look in the table
      -- for the results. This is basically similarityScore', but we replace recursive
      -- calls with calls in the table
      | x == y = score x y + simSco (i - 1) (j - 1) -- Letters are the same; Don't try to insert space but look for rest of word (exact match is max score for one column)
      | otherwise =
        maximum
          [ simSco i (j - 1) + score x '-',
            simSco (i - 1) j + score '-' y
          ]
      where
        x = s1 !! (i - 1) -- x and y are the Char at (i - 1) and (j - 1) in strings s1 and s2 respectively (i and j comes from length of strings, i.e. maxIndex + 1)
        y = s2 !! (j - 1)

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
optAlignments' :: String -> String -> [AlignmentType]
optAlignments' [] _ = [("", "")]
optAlignments' _ [] = [("", "")]
optAlignments' (s1 : s1s) (s2 : s2s) =
  maximaBy pairScore $
    attachHeads s1 s2 (optAlignments' s1s s2s) -- We don't insert any space here and find alignment for rest of the Strings
      ++ attachHeads s1 '-' (optAlignments' s1s (s2 : s2s)) -- We insert space instead first char of second string s2 and try to find the maximum for rest of s1 and the whole of s2
      ++ attachHeads '-' s2 (optAlignments' (s1 : s1s) s2s) -- Line above but we insert space instead of s1 instead

-- Optimized version of optAlignments
optAlignments :: String -> String -> [AlignmentType]
optAlignments _ _ = []

-- Uses a optAlignments function to create a neat output of optimal alignments
getOutputOptAlignments :: (String -> String -> [AlignmentType]) -> String -> String -> IO ()
getOutputOptAlignments optAl s1 s2 = do
  mapM_ printPair $ optAl s1 s2 -- mapM_ executes the IO action printPair for every part of oa, skipping the last empty IO action
  where
    printPair (x, y) = do
      print $ intersperse ' ' x
      print $ intersperse ' ' y

outputOptAlignments' = getOutputOptAlignments optAlignments'

outputOptAlignments = getOutputOptAlignments optAlignments