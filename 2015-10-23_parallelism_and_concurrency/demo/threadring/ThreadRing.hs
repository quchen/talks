{-# LANGUAGE NumDecimals #-}

-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- Contributed by Jed Brown with improvements by Spencer Janssen and Don Stewart
--
-- 503 threads are created with forkIO, with each thread
-- creating one synchronised mutable variable (MVar) shared with the
-- next thread in the ring. The last thread created returns an MVar to
-- share with the first thread. Each thread reads from the MVar to its
-- left, and writes to the MVar to its right.
--
-- Each thread then waits on a token to be passed from its neighbour.
-- Tokens are then passed around the threads via the MVar chain N times,
-- and the thread id of the final thread to receive a token is printed.
--
-- More information on Haskell concurrency and parallelism:
--   http://www.haskell.org/ghc/dist/current/docs/users_guide/lang-parallel.html
--

import Control.Monad
import Control.Concurrent

ringSize :: Int
ringSize = 3e6

tokenPasses :: Int
tokenPasses = ringSize

newNode :: MVar Int -- ^ Previous node's tag
        -> Int      -- ^ Thread ID
        -> IO (MVar Int)
newNode before index = do
  after <- newEmptyMVar
  _ <- forkIO (thread index before after)
  pure after

thread :: Int      -- ^ Thread ID
       -> MVar Int -- ^ Previous node's tag
       -> MVar Int -- ^ Following node's tag
       -> IO ()
thread index before after = go
  where
    go = do
        m <- takeMVar before
        when (m == 1) (print index)
        putMVar after $! m - 1
        when (m > 0) go

main :: IO ()
main = do
    putStrLn ("Creating thread ring of size " ++ show ringSize)
    a <- newEmptyMVar
    z <- foldM newNode a [2..ringSize]
    putStrLn ("Passing token " ++ show tokenPasses ++ " times")
    putMVar a tokenPasses
    thread 1 z a
