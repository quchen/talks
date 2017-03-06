module HaskellCheatSheet where


-- Basic values

true = 2 > 1

two = 1 + 1                         -- Infix notation

three = (+) 1 2                     -- Prefix notation

addOne = (+1)                       -- Infix operator section

oneTwoThree = [1,2,3]               -- Basic list

oneToTen = [1..10]                  -- Range

evens = [0,2..]                     -- Infinite list

evens' = [ n | n <- [0..], even n ] -- List comprehension

squares = [ n^2 | n <- [0..] ]

sixIsEven = 6 `elem` evens          -- Infix notation of names

lConcat = [1,2,3] ++ [4,5,6]        -- List concatenation

sConcat = "hello" ++ "world"        -- String concatenation

cons = 1 : [2,3,4]                  -- Prepending ("cons")



-- List functions

list1 = take 3 [1..]                -- = [1,2,3]

list2 = takeWhile (<= 5) [1..]      -- = [1,2,3,4,5]

list3 = drop 3 [1..10]              -- = [4,5,6,7,8,9,10]

list4 = dropWhile (<= 5) [1..10]    -- = [6,7,8,9,10]

list5 = length [1,3..100]           -- = 50

list6 = map (*2) [1..10]            -- = [2,4,6,8,10,12,14,16,18,20]

list7 = replicate 10 'a'            -- = "aaaaaaaaaa"

list8 = reverse [1..10]             -- = [10,9,8,7,6,5,4,3,2,1]



-- Function syntax

add1 x y = x + y         -- Fully expanded
add2     = (+)           -- Fully curried
add3     = \x y -> x + y -- Lambda

isEven n
    | n == 0 = True
    | n > 0  = not (isEven (n-1))
    | n < 0  = not (isEven (n+1))

fiboBad n | n <= 1 = 1
fiboBad n = fiboBad (n-1) + fiboBad (n-2)

fiboBetter n = fibonacci 0 1 !! n
  where
    fibonacci a b = a : fibonacci b (a+b)



-- Data types

data YesNo = Yes | No

data Tuple a b = Tuple a b

data Optional a = Absent | Some a

data List a = Nil | Cons a (List a)

data Tree a = Empty | Node a (Tree a) (Tree a)


type Triple a b c = Tuple a (Tuple b c)

type IntList = List Int



-- Pattern matching

if' :: YesNo -> a -> a -> a
if' Yes trueBranch _ = trueBranch
if' No _ falseBranch = falseBranch

isPresent (Some _) = True
isPresent Absent = False

safeHead :: List a -> Optional a
safeHead Nil = Absent
safeHead (Cons x xs) = Some x

flattenTree :: Tree a -> [a]
flattenTree Empty = []
flattenTree (Node x left right) = x : flattenTree left ++ flattenTree right


-- Various functions

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

mirrorTree :: Tree a -> Tree a
mirrorTree Empty = Empty
mirrorTree (Node x left right) = Node x (mirrorTree right) (mirrorTree left)

listToHaskellList :: List a -> [a]
listToHaskellList Nil = []
listToHaskellList (Cons x xs) = x : listToHaskellList xs

haskellListToList :: [a] -> List a
haskellListToList [] = Nil
haskellListToList (x:xs) = Cons x (haskellListToList xs)



-- Instance definitions

class Eq' a where
    (===) :: a -> a -> Bool
    x === y = not (x /== y)
    (/==) :: a -> a -> Bool
    x /== y = not (x === y)

instance Eq' a => Eq' (Optional a) where
    Absent === Absent = True
    Some x === Some y = x === y
    _x     === _y     = False

class Functor' f where
    fmap :: (a -> b) -> f a -> f b

instance Functor' Optional where
    fmap _ Absent = Absent
    fmap f (Some x) = Some (f x)

class Functor' f => Applicative' f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative' Optional where
    pure = Some
    Some f <*> Some x = Some (f x)
    _ <*> _ = Absent

class Applicative' m => Monad' m where
    (>>=) :: m a -> (a -> m b) -> m b

instance Monad' Optional where
    Absent >>= _ = Absent
    Some x >>= f = f x
