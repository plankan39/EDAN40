-- EDAN40 A2 String Alignment by Emil Eriksson (em5184er-s) && Lukas Elmlund (lu0804el-s)
import Data.List (intersperse)
import GhcPlugins (setStyleColoured)

-- Hardcoded values
scoreMatch, scoreMismatch, scoreSpace :: Int
string1, string2, string3, string4, string5, string6 :: [Char]
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"

string2 = "vintner"

string3 = "aferociousmonadatemyhamster"

string4 = "functionalprogrammingrules"

string5 = "bananrepubliksinvasionsarmestabsadjutant"

string6 = "kontrabasfiolfodralmakarmästarlärling"

-- Types
type AlignmentType = (String, String)

type ScoredAlignments = (Int, [AlignmentType])

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
similarityScore', similarityScore :: String -> String -> Int
similarityScore' [] _ = 0
similarityScore' _ [] = 0
similarityScore' (s1 : s1s) (s2 : s2s) =
  maximum
    [ similarityScore' s1s s2s + score s1 s2,
      similarityScore' s1s (s2 : s2s) + score s1 '-',
      similarityScore' (s1 : s1s) s2s + score '-' s2
    ]
-- Optimized similarityScore'
similarityScore [] _ = 0
similarityScore _ [] = 0
similarityScore s1 s2 = simSco (length s1) (length s2) -- Lengths are indexes, we go from the back
  where
    simSco i j = simTable !! i !! j -- We get all data from the table
    simTable = [[simEntry i j | j <- [0 ..]] | i <- [0 ..]] -- Generate table, where every entry is simEntry called on index

    -- Entry of our simTable, takes indexes and returns score of that entry
    simEntry :: Int -> Int -> Int
    simEntry 0 0 = 0
    -- These two are: We are at the border of the table
    simEntry 0 j = simSco 0 (j - 1) + score '-' (s2 !! (j - 1)) -- We compare an empty list to some string in the table
    simEntry i 0 = simSco (i - 1) 0 + score (s1 !! (i - 1)) '-'
    -- These are where the scoring is done, we recursively look in the table
    -- for the results. This is basically similarityScore', but we replace recursive
    -- calls with calls in the table
    simEntry i j =
      maximum
        [ simSco (i - 1) (j - 1) + score x y,
          simSco i (j - 1) + score x '-',
          simSco (i - 1) j + score '-' y
        ]
      where
        x = s1 !! (i - 1) -- x and y are the Char at (i - 1) and (j - 1) in strings s1 and s2 respectively (i and j comes from length of strings, i.e. maxIndex + 1)
        y = s2 !! (j - 1)

-- Attaches h1 and h2 to the list in fst and snd respectively
-- in a list of pairs
attachHeads, attachTails :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1 : xs, h2 : ys) | (xs, ys) <- aList]
attachTails h1 h2 aList = [(xs ++ [h1], ys ++ [h2]) | (xs, ys) <- aList] -- This is inneficient, structure differently?

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
optAlignments', optAlignments :: String -> String -> [AlignmentType]
optAlignments' [] _ = [("", "")]
optAlignments' _ [] = [("", "")]
optAlignments' (s1 : s1s) (s2 : s2s) =
  maximaBy pairScore $
    attachHeads s1 s2 (optAlignments' s1s s2s) -- We don't insert any space here and find alignment for rest of the Strings
      ++ attachHeads s1 '-' (optAlignments' s1s (s2 : s2s)) -- We insert space instead first char of second string s2 and try to find the maximum for rest of s1 and the whole of s2
      ++ attachHeads '-' s2 (optAlignments' (s1 : s1s) s2s) -- Line above but we insert space instead of s1 instead

-- Optimized version of optAlignments'
optAlignments [] _ = [("", "")]
optAlignments _ [] = [("", "")]
optAlignments s1 s2 = al $ optAli (length s1) (length s2)
  where
    al (_, alignments) = alignments
    optAli i j = optTable !! i !! j
    optTable = [[optEntry i j | j <- [0 ..]] | i <- [0 ..]]

    -- Builds scored alignments from the back
    addAli :: ScoredAlignments -> Int -> Char -> Char -> ScoredAlignments
    addAli (score1, ali) score2 k l = (score1 + score2, attachTails k l ali)

    -- i -> j -> (Score, Alignments)
    optEntry :: Int -> Int -> ScoredAlignments
    optEntry 0 0 = (0, [])
    optEntry 0 j = addAli (optAli 0 (j - 1)) (score '-' y) '-' y
      where
        y = s2 !! (j - 1)
    optEntry i 0 = addAli (optAli (i - 1) 0) (score x '-') x '-'
      where
        x = s1 !! (i - 1)
    optEntry i j =
      joinsa $ maximaBy sa
        [ addAli (optAli (i - 1) (j - 1)) (score x y) x y,
          addAli (optAli i (j - 1)) (score x '-') x '-',
          addAli (optAli (i - 1) j) (score '-' y) '-' y
        ]
      where
        -- Joins multiple ScoredAlignment tuples with same score to one ScoredAlignments
        joinsa :: [ScoredAlignments] -> ScoredAlignments
        joinsa sas = foldl (\(score, accAlis) (_, alis) -> (score, accAlis ++ alis)) (fst $ head sas, []) sas
        sa :: ScoredAlignments -> Int
        sa (score, _) = score
        x = s1 !! (i - 1)
        y = s2 !! (j - 1)

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