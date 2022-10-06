{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib (
    ClientAction(..),

    serialize,
    deserialize
) where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data ClientAction = Quit | Message Text
    deriving (Eq, Ord, Show, Generic)

clientActionToJson :: ClientAction -> Value
clientActionToJson Quit = String "quit"
clientActionToJson (Message msg) = object ["message" .= msg]

jsonToClientAction :: Value -> Parser ClientAction
jsonToClientAction v = quit v <|> message v
  where
    quit = withText "Quit" (\x -> if x == "quit" then pure Quit else empty)
    message = withObject "Message" $ \o -> do
        msg <- o .: "message"
        pure (Message msg)

instance ToJSON ClientAction where
instance FromJSON ClientAction where

deserialize :: BS.ByteString -> Either String ClientAction
deserialize = eitherDecodeStrict

serialize :: ClientAction -> BS.ByteString
serialize = BSL.toStrict . encode
