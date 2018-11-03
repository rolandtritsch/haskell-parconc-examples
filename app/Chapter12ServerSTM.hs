{-|
A simple network server. The server accepts connections from clients on port 44444.
If a client sends an integer n, then the service responds with the value of 2n.
If a client sends the string "end", then the server closes the connection.

We also allow the "factor" to be changed (and for that we need to introduce
some shared state between the threads (and we protect that shared state with
STM)).
-}

module Chapter12ServerSTM where

import System.IO (Handle, hClose, hPutStrLn, hGetLine, hSetBuffering, BufferMode(LineBuffering))
import Network (withSocketsDo, accept, listenOn, PortID(PortNumber))
import Text.Printf (printf, hPrintf)
import Control.Monad (forever)
import Control.Concurrent (forkFinally, ThreadId, myThreadId, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, TVar, readTVar, writeTVar)
import Control.Concurrent.STM.TChan (TChan, newTChan, writeTChan, readTChan)
import Control.Concurrent.Async (race)

port :: Int
port = 44444

talk :: Handle -> ThreadId -> TVar Integer -> IO ()
talk h mtid factor = do
  hSetBuffering h LineBuffering
  c <- atomically newTChan
  race (server h mtid factor c) (receive h c)
  return ()

receive :: Handle -> TChan String -> IO ()
receive h c = forever $ do
  line <- hGetLine h
  atomically $ writeTChan c line

server :: Handle -> ThreadId -> TVar Integer -> TChan String -> IO ()
server h mtid factor c = do
  f <- atomically $ readTVar factor
  hPrintf h "Current factor: %d ...\n" f
  loop f
 where
  loop f = do
    action <- atomically $ do
      f' <- readTVar factor
      if (f /= f') then return (newfactor f')
      else do
        l <- readTChan c
        return (command f l)
    action

  newfactor f = do
    hPrintf h "New factor: %d ...\n" f
    loop f

  command f s = case s of
    "end" -> do
      hPutStrLn h ("Thank you for using the Haskell doubling service. Shuting down the connection/thread/session ...")
    "quit" -> do
      hPutStrLn h ("Thank you for using the Haskell doubling service. Shutting down the server ...")
      killThread mtid
    '*':s -> do
      atomically $ writeTVar factor (read s :: Integer)
      loop f
    line -> do
      hPutStrLn h (show (f * (read line :: Integer)))
      loop f

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  mainThreadId <- myThreadId
  factor <- atomically $ newTVar 2
  printf "%s listening on port %d ...\n" (show mainThreadId) port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s:%s ...\n" host (show port)
    forkFinally (talk handle mainThreadId factor) (\_ -> do
                                                      putStrLn "Closing connection ..."
                                                      hClose handle
                                                  )
