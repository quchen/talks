import Network.Simple.TCP
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Lib

main :: IO ()
main = do
    putStrLn "Starting server"
    serve (Host "localhost") "8080" serverBody

serverBody :: (Socket, SockAddr) -> IO ()
serverBody (socket, sockAddr) = do
    putStrLn ("Hello, client " ++ show sockAddr)
    serverLoop socket
    putStrLn ("Bye, client " ++ show sockAddr)

serverLoop :: Socket -> IO ()
serverLoop socket = do
    raw <- recv socket 1000
    let input = case raw of
            Nothing -> Left "Client disconnected"
            Just bs -> deserialize bs

    case input of
        Left err -> putStrLn ("Error: " ++ err)
        Right Quit -> putStrLn "Client says goodbye"
        Right (Message msg) -> do
            T.putStrLn msg
            serverLoop socket
