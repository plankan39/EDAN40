module Chatterbot where

import Data.Char
import Data.Maybe (isJust)
import GHC (ApplicativeArg (xarg_app_arg_many))
import System.Random
import Utilities

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
  putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
  botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]

type PhrasePair = (Phrase, Phrase)

type BotBrain = [(Phrase, [Phrase])]

--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind bb = do
  r <- randomIO :: IO Float
  {- Create a 2D map of ID (nothing) and pick a random selection for that prompt,
  then create a function where all of those are applied to a function, and apply rules to that
  Finally apply this function to BotBrain bb and make monad of it

  For example, in
  [ <-- map works here
  ("", <-- id is applied to this via map2
      ["Speak up! I can't hear you."] <-- pick r is applied here via map2
  ),
  ("I need *",
      ["Why do you need * ?",
       "Would it really help you to get * ?",
       "Are you sure you need * ?"]),
  -- ...
  ]
  -}
  return $ rulesApply . map (map2 (id, pick r)) $ bb

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply trans wrds = k
  where Just k = orElse(transformationsApply "*" reflect trans wrds) (Just [])


reflect :: Phrase -> Phrase
reflect [] = []
reflect (p : ps) = k : reflect ps
  where
    Just k = orElse (lookup p reflections) (Just p)

reflections =
  [ ("am", "are"),
    ("was", "were"),
    ("i", "you"),
    ("i'm", "you are"),
    ("i'd", "you would"),
    ("i've", "you have"),
    ("i'll", "you will"),
    ("my", "your"),
    ("me", "you"),
    ("are", "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your", "my"),
    ("yours", "mine"),
    ("you", "me")
  ]

---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (== "quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN -}
rulesCompile _ = []

--------------------------------------

reductions :: [PhrasePair]
reductions =
  (map . map2)
    (words, words)
    [ ("please *", "*"),
      ("can you *", "*"),
      ("could you *", "*"),
      ("tell me if you are *", "are you *"),
      ("tell me who * is", "who is *"),
      ("tell me what * is", "what is *"),
      ("do you know who * is", "who is *"),
      ("do you know what * is", "what is *"),
      ("are you very *", "are you *"),
      ("i am very *", "i am *"),
      ("hi *", "hello *")
    ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply _ = id

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute w (s : ss) r
  | s == w = r ++ substitute w ss r
  | otherwise = s : substitute w ss r

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
--
-- If first of pattern is wildcard, check for match
-- If first of pattern is not wildcard, but is same as String first, check rest of string
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match _ p x
  | p == x = Just []
match wc (p : ps) (x : xs)
  | wc == p = orElse (singleWildcardMatch (p : ps) (x : xs)) (longerWildcardMatch (p : ps) (x : xs))
  | p == x = match wc ps xs
  | otherwise = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch [] _ = Nothing
singleWildcardMatch _ [] = Nothing
singleWildcardMatch (wc : ps) (x : xs)
  | ps == xs = Just [x]
  | Data.Maybe.isJust (match wc ps xs) = Just [x]
  | otherwise = Nothing
longerWildcardMatch [] _ = Nothing
longerWildcardMatch _ [] = Nothing
longerWildcardMatch (wc : ps) (x : xs) = mmap (x :) (match wc (wc : ps) xs)

-- Test cases --------------------

testPattern = "a=*;"

testSubstitutions = "32"

testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions

substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString

matchCheck = matchTest == Just testSubstitutions

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ [] (_, _) = Nothing
transformationApply _ _ _ ([], _) = Nothing
transformationApply _ _ _ (_, []) = Nothing
transformationApply wc f text (matchPattern, substitutePattern)
  | isJust mm = Just (substitute wc substitutePattern (f m))
  | otherwise = Nothing
  where
    mm = match wc matchPattern text
    Just m = mm

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply _ _ _ [] = Nothing
transformationsApply wc f (p : ps) text =
  orElse
    (transformationApply wc f text p)
    (transformationsApply wc f ps text)
