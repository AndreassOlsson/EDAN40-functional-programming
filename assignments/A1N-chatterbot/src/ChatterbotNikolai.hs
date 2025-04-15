module ChatterbotNikolai where

-- If you're not sure what this is, it's ok.
import           Control.Monad (mapM)
import           Data.Char
import           Data.Maybe
import           System.Random
import           Utilities

-- A pattern is a list of things
-- Where we have either a value or a wildcard
data PatternElem a
  = Wildcard
  | Item a
  deriving (Eq, Show)

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
makePair (Rule (pattern, templates)) = do
  r <- randomIO :: IO Float
  return (pattern, pick r templates)

-- We have the makePair function to be able to randomly choose between templates when the user
-- inputs a pattern. So Eliza does not return the same template every time.
rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply rules phrase =
  fromMaybe phrase (transformationsApply reflect rules phrase)

-- rules = each pattern template pair
-- We do transformationsApply on rules(list of patterns and templates) and the phrase,
-- and for the first match between a phrase and a pattern we transform that matched phrase
-- to its reflection using our reflections map. Otherwise we return Nothing.
-- fromMaybe returns the transformed phrase or the original phrase.
reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = map (\word -> fromMaybe word (lookup (map toLower word) reflections))

-- map(\word ->) applies the function to each word in the input Phrase
-- Maps the word to lowercase, looks up if the word is in reflections and returns
-- its reflection otherwise Nothing.
-- fromMaybe word returns the reflection or the same word based on lookup returned word
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
ruleCompile (pattern, templates) =
  Rule (starPattern (map toLower pattern), map starPattern templates)

-- We just map the pattern string toLowerCase and then we make it to a pattern with "*" as wildcard symbol.
-- Then for each string in the list we make it into a template with "*" as wildcard symbol.
-- Then we make a new Rule out of this.
--------------------------------------
-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- mkPattern '*' "Hi *!" => [Item 'H', Item 'i', Wildcard, Item '!']
mkPattern :: (Eq a) => a -> [a] -> Pattern a
{- TO BE WRITTEN -}
mkPattern wildcard list =
  Pattern
    (map
       (\x ->
          if x == wildcard
            then Wildcard
            else Item x)
       list)

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
reductionsApply reductions = fix (transformationsApply id reductions `try`)

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------
-- Replaces a wildcard in a template with the list given as the third argument
substitute :: (Eq a) => Template a -> [a] -> [a]
{- TO BE WRITTEN -}
substitute (Pattern []) _ = []
substitute (Pattern (Item x:ps)) substitution =
  x : substitute (Pattern ps) substitution
substitute (Pattern (Wildcard:ps)) substitution =
  substitution ++ substitute (Pattern ps) substitution

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: (Eq a) => Pattern a -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
match (Pattern []) [] = Just []
match (Pattern []) _ = Nothing
match (Pattern (Item x:ps)) (y:ys)
  | x == y = match (Pattern ps) ys
  | otherwise = Nothing
match (Pattern (Wildcard:ps)) xs =
  orElse
    (singleWildcardMatch (Pattern (Wildcard : ps)) xs)
    (longerWildcardMatch (Pattern (Wildcard : ps)) xs)
match _ [] = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch ::
     (Eq a) => Pattern a -> [a] -> Maybe [a]
singleWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  case match (Pattern ps) xs of
    Nothing -> Nothing
    Just _  -> Just [x]
--
singleWildcardMatch _ _ = Nothing -- Add this line to handle all other cases

{- TO BE WRITTEN -}
longerWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  mmap (x :) (match (Pattern (Wildcard : ps)) xs)
longerWildcardMatch _ _ = Nothing -- Handle all other cases

-------------------------------------------------------
-- Applying patterns transformations
--------------------------------------------------------
-- Helper function: Matches a pattern and applies the transformation
matchAndTransform :: (Eq a) => ([a] -> [a]) -> Pattern a -> [a] -> Maybe [a]
matchAndTransform transform pat = mmap transform . match pat

-- Applying a single pattern
transformationApply ::
     (Eq a) => ([a] -> [a]) -> [a] -> (Pattern a, Template a) -> Maybe [a]
{- TO BE WRITTEN -}
transformationApply f str (pattern, template) =
  mmap (substitute template . f) (match pattern str)

-- substitute template . f = \x -> substitute template (f x)
-- mmap is just a helper function for the Maybe monad and it returns Just[result from substitute template . f]
-- if there is a match, otherwise it returns Nothing immediately
-- This function takes just one pattern, tries to match it to the input str first,
-- then if it matches and returns ex: Just["Happy"] we apply f to the returned value.
-- The dot . stands for "apply f first". Since f is identity function id nothing changes.
-- We then substitute the wildcard in the template with the return value from the match function,
-- ex: "Happy" and then we return the template with the wildcard substituted otherwise Nothing.
-- Applying a list of patterns until one succeeds
transformationsApply ::
     (Eq a) => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
transformationsApply _ [] _ = Nothing -- If there are no more patterns to try we return Nothing
transformationsApply f (x:xs) str =
  orElse (transformationApply f str x) (transformationsApply f xs str)
--  Here is the recursion, we check if the first pattern matches the input string,
--  if it does match we return the template with the wildcard substituted.
--  if it does not match we recursively check the next pattern in the list.
--  The function f = id so it "does nothing"
--  We will return the template with the wildcard substituted or Nothing.
