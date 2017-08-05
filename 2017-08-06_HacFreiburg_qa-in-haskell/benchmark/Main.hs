module Main (main) where



import           Criterion.Main
import qualified Data.List      as L
import           System.Random

import qualified Prolude



randomList
    :: Int -- ^ Number of elements to generate
    -> IO [Int]
randomList n = do
    gen <- newStdGen
    pure (take n (randoms gen))

main :: IO ()
main = error "TODO: benchmark"
