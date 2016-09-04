module Main (main) where



import           Criterion.Main
import qualified Data.List      as L
import           System.Random

import qualified Prolude



main :: IO ()
main = defaultMain
    [ env (randomList 10) (\list ->
        bgroup "Sorting a random list"
            [ bench "Library reference"
                    (nf L.sort list)
            , bench "Prosort™"
                    (nf Prolude.proSort list)
            ] )
    , env (pure [1..10 :: Int]) (\list ->
        bgroup "Sorting a sorted list"
            [ bench "Library reference"
                    (nf L.sort list)
            , bench "Prosort™"
                    (nf Prolude.proSort list)
            ] )
    , env (pure (reverse [1..10 :: Int])) (\list ->
        bgroup "Sorting a reverse-sorted list"
            [ bench "Library reference"
                    (nf L.sort list)
            , bench "Prosort™"
                    (nf Prolude.proSort list)
            ] )
    ]

randomList
    :: Int -- ^ Number of elements to generate
    -> IO [Int]
randomList n = do
    gen <- newStdGen
    pure (take n (randoms gen))
