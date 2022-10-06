{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Control.Monad (unless)

import Lib ( serialize, ClientAction(Quit, Message) )

main :: IO ()
main = connect "localhost" (show 8080) clientBody

clientBody :: (Socket, SockAddr) -> IO ()
clientBody (socket, sockAddr) = do
    T.putStrLn "Client started!"
    loop
  where
    loop = do
        userInput <- T.getLine
        let payload = if userInput == ":quit" then Quit else Message userInput
        send socket (serialize payload)
        unless (payload == Quit) loop
