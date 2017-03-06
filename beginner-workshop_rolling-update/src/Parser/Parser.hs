{-# OPTIONS_GHC -Wall #-}

module Parser where

import Control.Monad

---------------------------------------------------------------

data Parser a = Parser { runParser :: String -> [(a, String)] }

parseUnique :: Parser a -> String -> Maybe a
parseUnique p input = case [result | (result, []) <- runParser p input] of
    [unique] -> Just unique
    _otherwise -> Nothing

failP :: Parser a
failP = Parser (\_ -> [])

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure x = Parser (\input -> [(x, input)])
    (<*>) = ap

instance Monad Parser where
    return = pure
    p >>= f = Parser (\input ->
        concat [runParser (f a) rest
               | (a, rest) <- runParser p input] )

orElse :: Parser a -> Parser a -> Parser a
p1 `orElse` p2 = Parser (\input -> runParser p1 input ++ runParser p2 input)

anyChar :: Parser Char
anyChar = Parser $ \input -> case input of
    [] -> []
    c:cs -> [(c, cs)]

---------------------------------------------------------------

many :: Parser a -> Parser [a]
many p = many1 p `orElse` pure []

many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many p
    pure (x:xs)

allOf :: [Parser a] -> Parser [a]
allOf = sequence

anyOf :: [Parser a] -> Parser a
anyOf = foldr orElse failP

between :: Parser a -> Parser b -> Parser c -> Parser c
between before after p = do
    _ <- before
    result <- p
    _ <- after
    pure result

---------------------------------------------------------------

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    c <- anyChar
    if p c then pure c
           else failP

char :: Char -> Parser Char
char x = satisfy (== x)

noneOf :: [Char] -> Parser Char
noneOf forbidden = satisfy (`notElem` forbidden)

digit :: Parser Char
digit = satisfy (`elem` "0123456789")

anyString :: Parser String
anyString = many anyChar

string :: String -> Parser String
string = traverse char

integer :: Parser Integer
integer = Parser reads -- Cheating a bit with a built-in

integerInRange :: Integer -> Integer -> Parser Integer
integerInRange lo hi = do
    i <- integer
    if lo <= i && i <= hi
        then pure i
        else failP

-- parse (parenthesized (anyChar >> parenthesized anyChar)) "(a(b))"
-- ==> [('b',"")]
parenthesized :: Parser a -> Parser a
parenthesized = between (char '(')
                        (char ')')
