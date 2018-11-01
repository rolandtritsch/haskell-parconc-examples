{-|
A simple network server. The server accepts connections from clients on port 44444.
If a client sends an integer n, then the service responds with the value of 2n.
If a client sends the string "end", then the server closes the connection.
-}

module Chapter12Server where

import System.IO (Handle, hClose, hPutStrLn, hGetLine, hSetBuffering, BufferMode(LineBuffering))
import Network (withSocketsDo, accept, listenOn, PortID(PortNumber))
import Text.Printf (printf)
import Control.Monad (forever)
import Control.Concurrent (forkFinally, ThreadId, myThreadId, killThread)

port :: Int
port = 44444

talk :: Handle -> ThreadId -> IO ()
talk h mtid = do
  hSetBuffering h LineBuffering
  loop
 where
  loop = do
    line <- hGetLine h
    if line == "end" then do
      hPutStrLn h ("Thank you for using the Haskell doubling service. Shuting down the connection/thread ...")
    else if line == "quit" then do
      hPutStrLn h ("Thank you for using the Haskell doubling service. Shutting down the server ...")
      killThread mtid
    else do
      hPutStrLn h (show (2 * (read line :: Integer)))
      loop

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  mainThreadId <- myThreadId
  printf "%s listening on port %d ...\n" (show mainThreadId) port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s ...\n" host (show port)
    forkFinally (talk handle mainThreadId) (\_ -> hClose handle)
