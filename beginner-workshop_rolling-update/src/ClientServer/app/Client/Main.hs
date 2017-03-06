-- Client
module Main where



import Control.Monad
import Network.Simple.TCP
import Util



main :: IO ()
main = let host = "127.0.0.1"
           port = 8000
       in startClient host port



startClient :: HostName -> Int -> IO ()
startClient host port
  = connect host (show port) (\(socket, remoteAddr) -> do
        putStrLn ("Connected to " ++ show remoteAddr)
        waitForUserInput socket )



waitForUserInput :: Socket -> IO ()
waitForUserInput socket = do
    putStrFlush "> "
    message <- getLine
    handleMessage socket message



handleMessage :: Socket -> String -> IO ()
handleMessage socket message = do
    let serialized = serialize (message ++ "\n")
    send socket serialized
    unless (message == "quit")
           (waitForUserInput socket)
