-- Server
module Main where



import           Control.Monad
import qualified Data.List          as L
import           Network.Simple.TCP
import           Text.Read
import           Util



main :: IO ()
main = do
    let host = "127.0.0.1"
        port = 8000
    putStrLn ("Starting server on " ++ host ++ ":" ++ show port)
    serverLoop host port

serverLoop :: HostName -> Int -> IO ()
serverLoop host port
  = serve (Host host) (show port) (\(socket, remoteAddr) -> do
        putStrLn ("Client " ++ show remoteAddr ++ " connected")
        handleSingleClient socket remoteAddr )



handleSingleClient :: Socket -> SockAddr -> IO ()
handleSingleClient socket remoteAddr = loop
  where
    loop = do
        input <- waitForClientInput socket
        case input of
            ConnectionError -> putStrLn "Connection error"
            ClientQuit -> putStrLn "Client quit"
            ValidMessage message -> do
                answerMessage remoteAddr message
                loop

data MessageResult
    = ConnectionError
    | ClientQuit
    | ValidMessage String

waitForClientInput :: Socket -> IO MessageResult
waitForClientInput socket = do
    chunk <- recv socket 1000
    pure (case chunk of
        Just msg -> case deserialize msg of
            x        | "quit" `L.isPrefixOf` x -> ClientQuit
            someText -> ValidMessage someText
        Nothing -> ConnectionError )

answerMessage :: SockAddr -> String -> IO ()
answerMessage clientId message
    | Just number <- readMaybe message = do
        putStrLn (client ++ " sent number " ++ show number)
        when (isPrime number) (putStrLn "Hey, that's prime!")
    | otherwise =
        putStrLn (client ++ " sent message " ++ show message)
  where
    client = "Client " ++ show clientId
