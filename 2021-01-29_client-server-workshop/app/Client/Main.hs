-- Client
module Main where



import Control.Monad
import Network.Simple.TCP
import Util
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TEnc



main :: IO ()
main = startClient "127.0.0.1" 8000



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



-- startClient2 :: HostName -> Int -> IO ()
-- startClient2 host port = connect host (show port) $ \(socket, remoteAddr) -> do
--     putStrLn ("Connected to " ++ show remoteAddr)
--     message <- T.getLine
--     -- serialize text to json???
--     let serialize text = ???
--     send socket message
