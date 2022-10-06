{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import Lib ( deserialize, ClientAction(Message, Quit) )

main :: IO ()
main = startServer

startServer :: IO a
startServer = do
    T.putStrLn "Starting server!"
    serve (Host "localhost") "8080" $ \(socket, address) -> do
        T.putStrLn ("Client connected as " <> T.pack (show address))
        handleClient socket
        putStrLn "Client disconnected"

handleClient :: Socket -> IO ()
handleClient socket = getPayload >>= handlePayload
  where
    getPayload = do
        raw <- recv socket 1000
        case raw of
            Nothing -> pure (Left "Network error: nothing received")
            Just bytes -> pure (deserialize bytes)
    handlePayload payload = case payload of
        Left err -> putStrLn ("Borked: " ++ err)
        Right Quit -> putStrLn "Client quit"
        Right (Message msg) -> do
            T.putStrLn msg
            handleClient socket

receiveAll = do
    raw <- recv socket 1000
    case raw of
        Nothing -> _
        Just content | BS.length
