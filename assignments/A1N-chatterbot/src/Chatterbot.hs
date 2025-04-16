module Chatterbot where

import           Data.Char
import           Data.Maybe
import           System.Random
import           Utilities

-- If you're not sure what this is, it's ok.
import           Control.Monad (mapM)

-- A pattern is a list of things
-- Where we have either a value or a wildcard
data PatternElem a
  = Wildcard
  | Item a
  deriving (Eq, Show)

-- describe :: PatternElem a -> String
-- describe Wildcard   = "This is a wildcard."
-- describe (Item _)   = "This is an item."
-- A pattern is a list of pattern elements
newtype Pattern a =
  Pattern [PatternElem a]
  deriving (Eq, Show)

-- Templates are the same as patterns
type Template a = Pattern a

-- A phrase is a list of string
type Phrase = [String]

newtype Rule =
  Rule (Pattern String, [Template String])
  deriving (Eq, Show)

type BotBrain = [Rule]

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
      if (not . endOfDialog) question
        then botloop
        else return ()

--------------------------------------------------------
-- This takes a brain, and returns a function
-- Which will take a phrase as an input and calculate the result
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind b = fmap rulesApply (mapM makePair b)

-- A rule maps a pattern to many answers, so we choose one
-- at random, and that's our bot
makePair :: Rule -> IO (Pattern String, Template String)
{- TO BE WRITTEN -}
-- Takes a rule (pattern + list of templates) and produces a tuple (pattern, template)
-- Uses randomRIO to select a random template from the list of templates
-- The random selection is why this function returns an IO type
makePair (Rule (pattern, templates)) = do
  i <- randomRIO (0, length templates - 1)
  return (pattern, templates !! i)

rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
-- Transforms a phrase using pattern transformations:
-- 1. Match the phrase with a pattern
-- 2. Reflect the match (convert between 1st and 2nd person)
-- 3. Substitute the reflected match in the target pattern
-- Use transformationsApply with reflect as the transformation function
rulesApply = undefined

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
-- Replace each word in the phrase with its corresponding reflection
-- For example "i will see my reflection" -> "you will see your reflection"
-- Use the reflections list to map words to their reflections
-- If a word isn't in the reflections list, keep it unchanged
reflect = undefined

reflections =
  [ ("am", "are")
  , ("was", "were")
  , ("i", "you")
  , ("i'm", "you are")
  , ("i'd", "you would")
  , ("i've", "you have")
  , ("i'll", "you will")
  , ("my", "your")
  , ("me", "you")
  , ("are", "am")
  , ("you're", "i am")
  , ("you've", "i have")
  , ("you'll", "i will")
  , ("your", "my")
  , ("yours", "mine")
  , ("you", "me")
  ]

---------------------------------------------------------------------------------
endOfDialog :: String -> Bool
endOfDialog = (== "quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map ruleCompile

ruleCompile :: (String, [String]) -> Rule
{- TO BE WRITTEN -}
-- Convert a tuple of (string pattern, list of string templates) into a Rule
-- The string pattern should be converted to a Pattern using stringToPattern or starPattern
-- Each template string should be converted to a Template
-- Patterns need to be in lowercase to match prepared input
ruleCompile = undefined

--------------------------------------
-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- mkPattern '*' "Hi *!" => [Item 'H', Item 'i', Wildcard, Item '!']
mkPattern :: Eq a => a -> [a] -> Pattern a
{- TO BE WRITTEN -}
-- Make a pattern from a wildcard element and a list
-- Replace any occurrence of the wildcard element with Wildcard
-- All other elements should be wrapped as Item
-- Example: mkPattern '*' "Hi *!" = Pattern [Item 'H', Item 'i', Item ' ', Wildcard, Item '!']
mkPattern = undefined

stringToPattern :: String -> String -> Pattern String
stringToPattern wc = mkPattern wc . words

starPattern :: String -> Pattern String
starPattern = stringToPattern "*"

reductions :: [(Pattern String, Pattern String)]
reductions =
  (map . map2)
    (starPattern, starPattern)
    [ ("please *", "*")
    , ("could you *", "*")
    , ("can you *", "*")
    , ("tell me if you are *", "are you *")
    , ("tell me who * is", "who is *")
    , ("tell me what * is", "what is *")
    , ("do you know who * is", "who is *")
    , ("do you know what * is", "what is *")
    , ("are you very *", "are you *")
    , ("i am very *", "i am *")
    , ("hi *", "hello *")
    ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [(Pattern String, Pattern String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
-- Apply reductions to simplify a phrase
-- Keep applying reductions until the phrase doesn't change anymore
-- Example: "I am very very tired" -> "I am tired"
-- Use transformationsApply to apply the reductions
-- The 'fix' function from Utilities.hs will be useful here to keep applying until no change
reductionsApply = undefined

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------
-- Replaces a wildcard in a template with the list given as the third argument
substitute :: Eq a => Template a -> [a] -> [a]
{- TO BE WRITTEN -}
-- Replace each wildcard in the template with the provided list
-- For example: substitute (mkPattern 'x' "3*cos(x) + 4 - x") "5.37" = "3*cos(5.37) + 4 - 5.37"
-- Need to traverse the template and replace Wildcard elements with the match
substitute = undefined

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => Pattern a -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
-- Try to match a pattern with a list
-- If they don't match, return Nothing
-- If they match but no wildcard was found, return Just []
-- If they match and a wildcard was found, return Just [extracted_match]
-- For multiple wildcards, only return the match of the first wildcard
-- Uses singleWildcardMatch and longerWildcardMatch for patterns with wildcards
match = undefined

-- Helper function to match
singleWildcardMatch, longerWildcardMatch ::
     Eq a => Pattern a -> [a] -> Maybe [a]
singleWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  case match (Pattern ps) xs of
    Nothing -> Nothing
    Just _  -> Just [x]

{- TO BE WRITTEN -}
-- Helper function for matching wildcards that span multiple items
-- Similar to singleWildcardMatch but:
-- 1. It doesn't consume the first wildcard of the pattern
-- 2. It extracts the match and prepends the current element to it
-- Tries to match the rest of the pattern with different lengths of matches for the wildcard
longerWildcardMatch = undefined

-------------------------------------------------------
-- Applying patterns transformations
--------------------------------------------------------
-- Helper function: Matches a pattern and applies the transformation
matchAndTransform :: Eq a => ([a] -> [a]) -> Pattern a -> [a] -> Maybe [a]
matchAndTransform transform pat = (mmap transform) . (match pat)

-- Applying a single pattern
transformationApply ::
     Eq a => ([a] -> [a]) -> [a] -> (Pattern a, Template a) -> Maybe [a]
{- TO BE WRITTEN -}
-- Apply a transformation to input text:
-- 1. Match the input with the pattern
-- 2. Apply the provided function to the match
-- 3. Substitute the transformed match into the template
-- Return Nothing if the match fails
-- The transformation function is typically 'reflect'
transformationApply = undefined

-- Applying a list of patterns until one succeeds
transformationsApply ::
     Eq a => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
-- Try each transformation in the list until one succeeds
-- Apply the provided function to the extracted match
-- Return Nothing if all transformations fail
-- Note the parameter order differs from transformationApply
-- Typically the transformation function is 'reflect'
transformationsApply = undefined
