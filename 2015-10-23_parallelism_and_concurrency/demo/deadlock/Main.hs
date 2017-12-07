module Main (main) where

import Control.Concurrent

main = do
    x <- newEmptyMVar
    takeMVar x