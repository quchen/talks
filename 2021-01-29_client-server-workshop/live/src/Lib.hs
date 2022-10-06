{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Text ( Text )
import qualified Data.Text as T
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as BSL
import GHC.Generics
import Data.Aeson
import Control.Applicative

data ClientAction = Quit | Message Text
    deriving (Show, Eq, Ord, Generic)

instance ToJSON ClientAction where
    toJSON Quit = String "Quit"
    toJSON (Message msg) = object ["Message" .= msg]

instance FromJSON ClientAction where
    parseJSON value = quit value <|> message value
      where
        quit = withText "Quit field expected" (\x ->
            if x == "Quit"
                then pure Quit
                else empty)
        message = withObject "Message field expected" (\obj -> do
            msg <- obj .: "Message"
            pure (Message msg))

serialize :: ClientAction -> ByteString
serialize = BSL.toStrict . encode

deserialize :: ByteString -> Either String ClientAction
deserialize = eitherDecode . BSL.fromStrict
