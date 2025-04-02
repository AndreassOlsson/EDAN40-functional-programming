-- Introduction to Haskell Basics

-- Comments in Haskell use double dashes
{- Or multi-line comments
   like this -}

-- Basic value definitions
-- In Haskell, we define values, not variables (they don't vary)
myName = "Alice"
myNumber = 42
myPi = 3.14159

-- Function definition - no parentheses, arguments separated by spaces
-- The function body follows the = sign
add x y = x + y

-- Function calls also don't need parentheses
result = add 5 7  -- equals 12

-- Type annotations - explicitly declaring types
-- Format: name :: Type
age :: Int
age = 30

-- String concatenation
greeting :: String
greeting = "Hello, " ++ myName ++ "!"

-- Boolean logic
isAdult :: Bool
isAdult = age >= 18

-- Conditional expressions
welcomeMessage :: String
welcomeMessage = if isAdult
                 then "Welcome to the site!"
                 else "You must be 18 or older."

-- Lists (homogeneous collections)
numbers = [1, 2, 3, 4, 5]
names = ["Alice", "Bob", "Charlie"]

-- Tuples (heterogeneous collections)
person = ("David", 42)
point = (10, 20, 30)  -- A 3D point

-- List operations
firstNumber = head numbers  -- 1
restOfNumbers = tail numbers  -- [2,3,4,5]
secondNumber = numbers !! 1  -- 2 (zero-indexed)
moreNumbers = numbers ++ [6, 7, 8]  -- Concatenation

-- Main function - entry point for Haskell programs
main :: IO ()
main = do
    putStrLn "Hello, Haskell World!"
    putStrLn greeting
    putStrLn welcomeMessage
    print (add 10 32)
    print numbers
