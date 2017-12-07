{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.DeepSeq
import Control.Exception
import Control.Parallel.Strategies
import Numeric.Natural
import System.Environment
import Primes

filterPrimes :: [Natural] -> [Natural]
filterPrimes = filter isPrime

filterPrimes' :: [Natural] -> [Natural]
filterPrimes' = map snd . filter fst . map (\n -> (isPrime n, n))

filterPrimesParallel :: [Natural] -> [Natural]
filterPrimesParallel = map snd . filter fst . parallelize . map (\n -> (isPrime n, n))
  where
    parallelize :: [(a, b)] -> [(a, b)]
    parallelize = withStrategy tupleFstList

    tupleFstList :: Strategy [(a, b)]
    tupleFstList = parList parFst

    parFst :: Strategy (a,b)
    parFst = parTuple2 rseq r0

main :: IO ()
main = do
    let candidates = take 1e4 [1e13..]
    getArgs >>= \case
        ["seq"] -> do
            putStrLn "Running sequentially"
            let primes = filterPrimes' candidates
            _ <- evaluate (force primes)
            pure ()
        ["par"] -> do
            putStrLn "Running in parallel"
            let primes = filterPrimesParallel candidates
            _ <- evaluate (force primes)
            pure ()
        _else -> print "Usage: $0 seq|par"
