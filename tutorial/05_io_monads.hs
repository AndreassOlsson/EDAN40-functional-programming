-- Input/Output and Monads in Haskell

-- Basic IO operations
-- IO is how Haskell handles side effects in a pure functional way

-- Simple output
printGreeting :: String -> IO ()
printGreeting name = putStrLn ("Hello, " ++ name ++ "!")

-- Reading input
getName :: IO String
getName = do
    putStr "What is your name? "  -- putStr doesn't add newline
    getLine  -- Reads a line of input

-- Combining IO actions with do notation
greetUser :: IO ()
greetUser = do
    putStrLn "Welcome to our Haskell program!"
    name <- getName  -- Extract the value from IO String
    printGreeting name

-- Working with files
readFromFile :: IO ()
readFromFile = do
    contents <- readFile "example.txt"  -- Reads entire file
    putStrLn "File contents:"
    putStrLn contents

writeToFile :: String -> IO ()
writeToFile text = do
    writeFile "output.txt" text  -- Writes to file (overwrites)
    putStrLn "Data has been written to output.txt"

appendToFile :: String -> IO ()
appendToFile text = do
    appendFile "output.txt" (text ++ "\n")  -- Appends to file
    putStrLn "Data has been appended to output.txt"

-- Introduction to Monads
-- Maybe monad example (handles potential failure)
findUser :: String -> Maybe String
findUser "admin" = Just "Administrator"
findUser "guest" = Just "Guest User"
findUser _ = Nothing  -- Not found

getPermissions :: String -> Maybe [String]
getPermissions "Administrator" = Just ["read", "write", "execute"]
getPermissions "Guest User" = Just ["read"]
getPermissions _ = Nothing

-- Using do notation with Maybe monad
checkUserPermissions :: String -> Maybe [String]
checkUserPermissions username = do
    user <- findUser username       -- If findUser returns Nothing, the whole
    permissions <- getPermissions user  -- expression evaluates to Nothing
    return permissions              -- "return" wraps the value in the monad

-- The same function using explicit bind
checkUserPermissions' :: String -> Maybe [String]
checkUserPermissions' username =
    findUser username >>= \user ->
    getPermissions user

-- List monad example
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = do
    x <- xs          -- For each x in xs
    y <- ys          -- For each y in ys
    return (x, y)    -- Create a pair

-- The same operation using list comprehension
cartesianProduct' :: [a] -> [b] -> [(a, b)]
cartesianProduct' xs ys = [(x, y) | x <- xs, y <- ys]

main :: IO ()
main = do
    greetUser
    
    -- Example of using Maybe monad
    let adminPerms = checkUserPermissions "admin"
    let guestPerms = checkUserPermissions "guest"
    let unknownPerms = checkUserPermissions "unknown"
    
    putStrLn "\nUser permissions:"
    print adminPerms
    print guestPerms
    print unknownPerms
    
    -- Example of using List monad
    putStrLn "\nCartesian product example:"
    print (cartesianProduct [1,2] ["a","b"])
    
    -- Uncomment to test file operations (after creating example.txt)
    -- readFromFile
    -- writeToFile "Hello from Haskell!"
    -- appendToFile "This is a new line."
