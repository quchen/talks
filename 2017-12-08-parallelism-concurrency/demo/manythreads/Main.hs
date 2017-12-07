#!/usr/bin/env stack
{- stack
    --resolver lts-9.0
    --install-ghc
    runghc
    --package async
-}

{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Foldable
import           System.Environment

main :: IO ()
main = do
    numThreads <- fmap (read . head) getArgs
    counter <- newEmptyMVar

    putStrLn ("Forking " ++ show numThreads ++ " threads")
    threads <- replicateM numThreads (async (worker counter))

    putStrLn "Start the counting cascade"
    putMVar counter 0
    traverse_ wait threads

    result <- readMVar counter
    putStrLn ("Counter: " ++ show result)

worker :: MVar Int -> IO ()
worker counter = modifyMVar_ counter (\x -> pure $! x+1)
