module Parser
  ( module CoreParser
  , T
  , digit
  , digitVal
  , chars
  , letter
  , err
  , lit
  , number
  , iter
  , accept
  , require
  , token
  , spaces
  , word
  , (-#)
  , (#-)
  ) where

import           CoreParser
import           Data.Char
import           Prelude    hiding (fail, return)

infixl 7 -#, #-
type T a = Parser a

err :: String -> Parser a
err message cs = error (message ++ " near " ++ cs ++ "\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []
  where
    cons (a, b) = a : b

(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd
  where
    snd (_, y) = y

(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst
  where
    fst (x, _) = x

-- | Consume zero or more spaces or comments
spaces :: Parser String
spaces = iter (spaceChar ! comment) >-> concat
  where
    spaceChar = char ? isSpace >-> (: [])
    comment = accept "--" -# iter (char ? (/= '\n')) #- lit '\n' >-> const ""

-- | Token parser: runs p and then skips trailing spaces/comments
token :: Parser a -> Parser a
token m = m #- spaces

-- | Parser for a single alphabetical letter
letter :: Parser Char
letter = char ? isAlpha

-- | Parse a fixed number of characters
chars :: Int -> Parser String
chars n
  | n <= 0 = return []
  | otherwise = char # chars (n - 1) >-> cons
  where
    cons (c, cs) = c : cs

-- | Accept a specific string, no error on failure
accept :: String -> Parser String
accept w = token (chars (length w)) ? (== w)

-- | Require a specific string, or report an error
require :: String -> Parser String
require w = accept w ! err ("\"" ++ w ++ "\" expected")

-- | Literal character parser with trailing space skip
lit :: Char -> Parser Char
lit c = token (char ? (== c))

-- | Parser for digits and numeric literals
digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\d -> number' (10 * n + d)) ! return n

number :: Parser Integer
number = token (digitVal #> number')

-- Assuming word parser is needed based on export list
word :: Parser String
word = token (letter # iter (char ? isAlphaNum) >-> cons)
  where
    cons (c, cs) = c : cs
