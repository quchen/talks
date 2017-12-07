#!/usr/bin/env stack
{- stack
    --resolver lts-9.0
    --install-ghc
    runghc
-}

module Main (main) where

import           Control.Concurrent

main :: IO ()
main = do
    x <- newEmptyMVar
    takeMVar x
