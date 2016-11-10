{-# LANGUAGE OverloadedStrings #-}

module Lib where



import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty   as JsonEnc
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Vector                (Vector)
import           System.IO.Unsafe           (unsafePerformIO)



pretty :: ToJSON a => a -> IO ()
pretty = BSL8.putStrLn . JsonEnc.encodePretty

{-# NOINLINE twitterExample #-}
twitterExample :: Value
twitterExample = unsafePerformIO $ do
    bs <- BSL.readFile "tweets.json"
    let Just json = decode bs
    pure json


twitterView l = toListOf l twitterExample & pretty

example1 :: Traversal' Value Value
example1 = key "search_metadata"

example2 :: Traversal' Value Value
example2 = key "statuses"

example3 :: Traversal' Value (Vector Value)
example3 = key "statuses" . _Array

example4 :: Traversal' Value Value
example4 = key "statuses" . _Array . traverse

example5 :: Traversal' Value Value
example5 = key "statuses" . _Array . traverse . key "text"


longestTweet = maximumOf (key "statuses" . _Array . traverse . key "text" . to show) twitterExample
