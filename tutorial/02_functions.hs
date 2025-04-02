-- Advanced Functions in Haskell

-- Function definition with type signature
add :: Int -> Int -> Int  -- Takes two Ints and returns an Int
add x y = x + y

-- Higher-order functions (functions that take functions as arguments)
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- Example: applyTwice (+3) 10 = 16

-- Lambda expressions (anonymous functions)
-- Syntax: \args -> expression
multiplyBy2 = \x -> x * 2

-- Using a lambda in a higher-order function
incrementAll = map (\x -> x + 1) [1, 2, 3]  -- [2, 3, 4]

-- Partial application
-- If we don't provide all arguments, we get a new function
add5 = add 5  -- This is a function that adds 5 to its argument
-- Usage: add5 10 = 15

-- Function composition using the . operator
-- (f . g) x = f (g x)
negate2Times :: Int -> Int
negate2Times = negate . negate  -- Equivalent to \x -> negate (negate x)

-- Infix function application using $
-- f $ x = f x
-- Useful to avoid parentheses
result = sum $ filter (>5) [1..10]
-- Without $: result = sum (filter (>5) [1..10])

-- Guards - a way to test multiple conditions
absoluteValue :: Int -> Int
absoluteValue n
    | n < 0     = -n
    | otherwise = n

-- Pattern matching
factorial :: Int -> Int
factorial 0 = 1               -- Base case
factorial n = n * factorial (n-1)  -- Recursive case

-- Where clause - local definitions
circleArea :: Float -> Float
circleArea r = pi * r * r
  where
    pi = 3.14159  -- Local definition of pi

-- Let expressions - another way for local definitions
cylinderVolume :: Float -> Float -> Float
cylinderVolume r h =
  let area = pi * r * r
      pi = 3.14159
  in area * h

-- Case expressions
describeList :: [a] -> String
describeList xs = case xs of
                    [] -> "Empty"
                    [x] -> "Singleton"
                    xs -> "Longer list"

main :: IO ()
main = do
    putStrLn "Examples of Haskell functions:"
    print (applyTwice (+3) 10)
    print (map multiplyBy2 [1, 2, 3, 4])
    print (add5 10)
    print (negate2Times 5)
    print (absoluteValue (-42))
    print (factorial 5)
    print (circleArea 5)
    print (cylinderVolume 2 10)
    print (describeList [1, 2, 3])
