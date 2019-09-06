-- So we can safely leave »gaps« in the code to fill out without the compiler
-- whining about them ;-)
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

-- Missing top-level type signatures should not concern us in the very
-- beginning. Let inference do the work for us!
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Exercises where


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
fizzy = _implement_me

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
cons2 = _implement_me

-- replicate 3 'a' == "aaa"
replicate = _implement_me

-- repeat x == [x,x,x,x,x,x,x,…]
repeat = _implement_me

-- cycle [a,b,c] == [a,b,c,a,b,c,a,b,c, …]
cycle = _implement_me

-- Extract `len` elements from a list, starting with the `start`th one.
--
-- >>> slice 2 4 [0..10]
-- [2,3,4,5]
--
-- Make sure to handle the edge cases!
slice start len xs = _implement_me

-- Project Euler #1:
-- Find the sum of all the multiples of 3 or 5 below 1000.
--
-- Might help:
--   - sum
--   - filter (\x -> x > 3) [1,2,3,4,5] = [4,5]
--   - [1..5]
--   - True || False
projectEuler1 = _implement_me

map = _fill_the_gaps "map f list = [ ??? | ??? <- xs ]"



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
sumOfEvenSquares = _todo




-- #############################################################################
-- Functions, pattern matching
-- #############################################################################

-- Convert a function taking a tuple to a function taking two arguments.
--
-- >>> curry' fst 1 2
-- 1
curry' :: ((a, b) -> c) -> a -> b -> c
curry' = _yum_yum

-- Inverse of curry: convert a function taking multiple arguments to one taking
-- a tuple.
--
-- >>> uncurry' (*) (3,4)
-- 12
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' = _ugh

-- Get the second element of a list, if there is one.
--
-- >>> head2 [1..10]
-- Just 2
-- >>> head2 [1]
-- Nothing
head2 :: [a] -> Maybe a
head2 = _todo



data Expr = Literal Integer
          | Add Expr Expr

myExpr :: Expr
myExpr = Add (Literal 1) (Add (Literal 2) (Literal 3))

sumOfLiterals :: Expr -> Integer
sumOfLiterals (Literal x) = _todo
sumOfLiterals _           = _todo

numberOfAddNodes :: Expr -> Integer
numberOfAddNodes = _todo

listOfLiterals :: Expr -> [Integer]
listOfLiterals = _todo


-- #############################################################################
-- Type classes
-- #############################################################################

class BoolIsh a where
    isTrue :: a -> Bool

    isFalse :: a -> Bool

instance BoolIsh Bool where
    isTrue = _todo
    isFalse = _todo

instance BoolIsh (Maybe a) where
    isTrue = _todo
    isFalse = _todo



-- #############################################################################
-- Monoids
-- #############################################################################

-- Concatenate a list of things that have a Monoid instance.
--
-- >>> mconcat' [[1,2,3], [4,5], [6]]
-- [1,2,3,4,5,6]
mconcat' :: Monoid m => [m] -> m
mconcat' = _todo




-- #############################################################################
-- Guards
-- #############################################################################

-- Many solutions are possible, such as
--   - Using guards
--   - Pattern matching
--   - Using the built-in `product`
factorial :: Integer -> Integer
factorial = _todo





-- #############################################################################
-- Mergesort!
-- #############################################################################

mergesort :: Ord a => [a] -> [a]
mergesort = _todo
  where
    -- Merge two ordered lists into one
    merge :: Ord a => [a] -> [a] -> [a]
    merge = _todo

    -- Split a list into equally sized halves (± 1 element)
    splitIntoHalves :: [a] -> ([a], [a])
    splitIntoHalves = _todo


-- Get the first element matching the predicate
find' :: (a -> Bool) -> [a] -> Maybe a
find' = _todo


-- Pair entries up, stop when one list is empty
-- e.g. zip' [1..] "abc" ==> [(1,'a'), (2,'b'), (3,'c')]
zip' :: [a] -> [b] -> [(a,b)]
zip' = _todo

-- combine list elements using a function
--
-- >>> zipWith (+) [1,2,3] [10,20,30]
-- [11,22,33]
--
-- NB: zip = zipWith (,)
--     zipWith' f = map f . zip
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' = _todo

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
runAll = _todo



-- Given
-- Use `fmap` for IO to implement this function!
-- Useful helpers:
--   - readFile :: String -> IO String
--   - length :: String -> Int
getFileLength :: String -> IO Int
getFileLength = _todo





-- #############################################################################
-- Laziness
-- #############################################################################


-- Keep only every other element of a possibly infinite list.
everyOther :: [a] -> [a]
everyOther = _todo

-- Split a list into halves of equal length (±1).
-- Easy using `length`, tricky so it works even on infinite lists.
splitAtMiddle :: [a] -> ([a], [a])
splitAtMiddle = _a_bit_tricky





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
    _todo
