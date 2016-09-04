{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import qualified Data.List as L

import Prolude

import Test.Tasty
import Test.Tasty.HUnit      as HU
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC



main :: IO ()
main = defaultMain (options testsuite)

options :: TestTree -> TestTree
options = quickcheckOptions . smallcheckOptions
  where
    quickcheckOptions =
          localOption (QuickCheckShowReplay False)
    smallcheckOptions =
          localOption (SmallCheckDepth 5)

testsuite :: TestTree
testsuite =
    testGroup "proSort"
        [ testGroup "Quickcheck"
            [ sortIdempotency
            , sortHomomorphism
            ]
        , testGroup "HUnit"
            [ sortExample ]
        , testGroup "Smallcheck"
            [ onlyOneEmpty
            , onlyOneFixedPoint ]
        ]

sortIdempotency :: TestTree
sortIdempotency = QC.testProperty "proSort . proSort = proSort" test
  where
    test :: [Int] -> Bool
    test xs = proSort (proSort xs) == proSort xs

sortHomomorphism :: TestTree
sortHomomorphism = QC.testProperty "proSort xs ++ proSort ys = proSort (xs ++ ys)" test
  where
    test :: QC.Property
    test = QC.forAllShrink arbitrary shrink (\(xs,ys) ->
        let _ = xs :: [Int]
        in proSort xs ++ proSort ys == proSort (xs ++ ys) )

sortExample :: TestTree
sortExample = testCase "example sort" test
  where
    test :: Assertion
    test = do
        let input = [1,2,3,5,2,4,3,4,7 :: Int]
            actual = proSort input
            expected = L.sort input
        assertEqual "Oh no" expected actual

onlyOneEmpty :: TestTree
onlyOneEmpty = SC.testProperty "Only [] proSorts to []" test
  where
    test = SC.existsUnique (\xs ->
        let _ = xs :: [Int]
        in proSort xs == [] )

onlyOneFixedPoint :: TestTree
onlyOneFixedPoint = SC.testProperty "Only [] proSorts to itself" test
  where
    test = SC.existsUnique (\xs ->
        let _ = xs :: [Int]
        in proSort xs == xs )
