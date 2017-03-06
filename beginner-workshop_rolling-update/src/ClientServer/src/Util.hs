module Util where

import System.IO (hFlush, stdout)
import qualified Data.ByteString.Char8 as BS8

serialize :: String -> BS8.ByteString
serialize = BS8.pack

deserialize :: BS8.ByteString -> String
deserialize = BS8.unpack

numBytes :: BS8.ByteString -> Int
numBytes = BS8.length

putStrFlush :: String -> IO ()
putStrFlush s = do
    putStr s
    hFlush stdout

isPrime :: Integer -> Bool
isPrime n
    | n < 0 = isPrime (-n)
    | n <= 1 = False
    | n == 2 = True
    | otherwise = all (\x -> mod n x > 0) (2:[3,5..n `div` 2])
