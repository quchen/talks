-- So we can safely leave »gaps« in the code to fill out without the compiler
-- whining about them ;-)
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

-- Missing top-level type signatures should not concern us in the very
-- beginning. Let inference do the work for us!
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Solutions where


import Text.Read

-- #############################################################################
-- If
-- #############################################################################

-- Fizzbuzz printer
--
-- >>> fizzy 2
-- "2"
-- >>> fizzy 3
-- "Fizz"
-- >>> fizzy 15
-- "FizzBuzz"
--
-- Helpful definitions:
--
--   - if … then … else
--   - mod 10 4  -->  2
--   - [1..5]  -->  [1,2,3,4,5]
fizzy :: Int -> String
fizzy x
    | mod x 15 == 0 = "FizzBuzz"
    | mod x 5 == 0 = "Buzz"
    | mod x 3 == 0 = "Fizz"
    | otherwise = show x

-- #############################################################################
-- Lists
-- #############################################################################

-- Prepend (»cons«) two elements to the front of a list. Hint: (:) does it for
-- a single element,
--
-- >>> 1 : [2,3]
-- [1,2,3]
--
-- >>> cons2 1 2 [3,4,5]
-- [1,2,3,4,5]
cons' x y list = x : y : list

-- replicate 3 'a' == "aaa"
replicate' 0 _ = []
replicate' counter x = x : replicate' (counter-1) x

-- repeat x == [x,x,x,x,x,x,x,…]
repeat' x = x : repeat' x

-- cycle [a,b,c] == [a,b,c,a,b,c,a,b,c, …]
cycle' list = list ++ cycle' list

-- Extract `len` elements from a list, starting with the `start`th one.
--
-- >>> slice 2 4 [0..10]
-- [2,3,4,5]
--
-- Make sure to handle the edge cases!
-- slice start len (x:xs) = slice (start-1) len xs
-- slice 0     len (x:xs) = x : slice 0 (len-1) xs
-- slice 0     0   (x:xs) = []
-- slice _     _   []   = []
slice start len = take len . drop start

-- Project Euler #1:
-- Find the sum of all the multiples of 3 or 5 below 1000.
--
-- Might help:
--   - sum
--   - filter (\x -> x > 3) [1,2,3,4,5] = [4,5]
--   - [1..5]
--   - True || False
projectEuler1 = sum (filter(\x -> mod x 3 == 0 || mod x 5 == 0) [1..999])

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs



-- #############################################################################
-- Function composition
-- #############################################################################

-- Find the sum of squares of even numbers from 1 to 99.
--   - even            -- Check whether a number is even
--   - filter p        -- Drop all numbers for which `p x` is `False`
--   - map (\x -> x^2) -- square all numbers in a list
--   - sum             -- Sum up a list
--
-- Try solving it in multiple ways!
--   - with a chain of ».«
--   - with a list comprehension
sumOfEvenSquares = (sum . map(\x -> x^2) . filter even) [1..99]




-- #############################################################################
-- Functions, pattern matching
-- #############################################################################

-- Convert a function taking a tuple to a function taking two arguments.
--
-- >>> curry' fst 1 2
-- 1
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f p1 p2 = f (p1, p2)

-- Inverse of curry: convert a function taking multiple arguments to one taking
-- a tuple.
--
-- >>> uncurry' (*) (3,4)
-- 12
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

-- Get the second element of a list, if there is one.
--
-- >>> head2 [1..10]
-- Just 2
-- >>> head2 [1]
-- Nothing
head2 :: [a] -> Maybe a
head2 [] = Nothing
head2 (_:y:xs) = Just y



data Expr = Literal Integer
          | Add Expr Expr

myExpr :: Expr
myExpr = Add (Literal 1) (Add (Literal 2) (Literal 3))

sumOfLiterals :: Expr -> Integer
sumOfLiterals (Literal x) = x
sumOfLiterals (Add left right) = sumOfLiterals left + sumOfLiterals right

numberOfAddNodes :: Expr -> Integer
numberOfAddNodes (Literal _) = 0
numberOfAddNodes (Add left right) = 1 + numberOfAddNodes left + numberOfAddNodes right

listOfLiterals :: Expr -> [Integer]
listOfLiterals (Literal x) = [x]
listOfLiterals (Add left right) = listOfLiterals left ++ listOfLiterals right


-- #############################################################################
-- Monoids
-- #############################################################################

-- Concatenate a list of things that have a Monoid instance.
--
-- >>> mconcat' [[1,2,3], [4,5], [6]]
-- [1,2,3,4,5,6]
mconcat' :: Monoid m => [m] -> m
mconcat' [] = mempty
mconcat' (x:xs) = mappend x (mconcat' xs)




-- #############################################################################
-- Guards
-- #############################################################################

-- Many solutions are possible, such as
--   - Using guards
--   - Pattern matching
--   - Using the built-in `product`
factorial :: Integer -> Integer
factorial x
  | x <= 0 = 0
  | x == 1 = 1
  | otherwise = x * factorial (x-1)





-- #############################################################################
-- Mergesort!
-- #############################################################################

mergesort :: Ord a => [a] -> [a]
mergesort xs
  | length xs == 0 = []
  | length xs == 1 = xs
  | otherwise = let (ys, zs) = splitIntoHalves xs
    in merge (mergesort ys) (mergesort zs)
  where
    -- Merge two ordered lists into one
    merge :: Ord a => [a] -> [a] -> [a]
    merge (x:xs) (y:ys) = if x < y then x:merge xs (y:ys) else y:merge (x:xs) ys
    merge [] ys = ys
    merge xs [] = xs

    -- Split a list into equally sized halves (± 1 element)
    splitIntoHalves :: [a] -> ([a], [a])
    splitIntoHalves (xs) = splitAt (div (length xs) 2) xs


-- Get the first element matching the predicate
find' :: (a -> Bool) -> [a] -> Maybe a
find' predicate (y:ys) = if predicate y then Just y else find' predicate ys
find' predicate [] = Nothing


-- Pair entries up, stop when one list is empty
-- e.g. zip' [1..] "abc" ==> [(1,'a'), (2,'b'), (3,'c')]
zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)
zip' _ _ = []

-- combine list elements using a function
--
-- >>> zipWith (+) [1,2,3] [10,20,30]
-- [11,22,33]
--
-- NB: zip = zipWith (,)
--     zipWith' f = map f . zip
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = [(f x y)] ++ zipWith f xs ys
zipWith' _ _ _ = []


-- Apply each function to the value
--
-- >>> applyAllTo 3 [even, odd, (> 3)]
-- [False, True, False]
applyAllTo :: a -> [a -> b] -> [b]
applyAllTo = _todo




-- #############################################################################
-- IO
-- #############################################################################

-- »Run all the actions contained in a list.«
--
-- >>> runAll [putStrLn "hello", putStrLn "world"]
-- hello
-- world
runAll :: [IO a] -> IO ()
runAll [] = return ()
runAll (x:xs) = do 
  _ <- x
  runAll xs



-- Given
-- Use `fmap` for IO to implement this function!
-- Useful helpers:
--   - readFile :: String -> IO String
--   - length :: String -> Int
getFileLength :: FilePath -> IO Int
getFileLength name = (fmap length . readFile) name





-- #############################################################################
-- Laziness
-- #############################################################################


-- Keep only every other element of a possibly infinite list.
everyOther :: [a] -> [a]
everyOther (x:y:xs) = x:everyOther xs
everyOther _ = []

-- Split a list into halves of equal length (±1).
-- Easy using `length`, tricky so it works even on infinite lists.
splitAtMiddle :: [a] -> ([a], [a])
splitAtMiddle xs = helper xs (everyOther xs)
  where 
    helper (x:xs) (y:ys) = let (a, b) = helper xs ys
      in (x:a, b)
    helper xs [] = ([], xs)
    helper _ _ = ([], [])

-- #############################################################################
-- Guessing game
-- #############################################################################

-- Given a secret number, let the user guess it, and tell him whether his guess
-- is too large or too small, until he finds the right number.
--
-- These might help:
--   - getLine reads a string from the user
--   - readMaybe tries to parse a string to e.g. an Integer
--   - compare x y ==> LT if x<y, GT if x>y, EQ if x=y
--   - putStrLn "prints strings"
--   - "conca" ++ "tenate" ++ "strings"
guessingGame :: Integer -> IO ()
guessingGame number = do
      putStrLn "Guess your number"
      guess <- getLine
      case readMaybe guess of
        Nothing -> do
          putStr "Please enter a number"
          guessingGame number
        Just guessInt -> 
          case compare number guessInt of
            LT -> do
              putStrLn "You guess is too high"
              guessingGame number
            GT -> do
              putStrLn "Your guess is too low"
              guessingGame number
            EQ -> putStrLn "You cheated!"

