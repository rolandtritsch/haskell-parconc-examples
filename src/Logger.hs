{-|
The Logger is just an MVar that we use as a channel for communication
with the logging thread. Requests are made by placing a LogCommand in
the MVar, and the logging thread will process requests one at a time
by taking them from the MVar.

There are two kinds of requests that we can make, and so LogCommand is
a data type with two constructors. The first, Message, is straightforward;
it simply contains a String that we want to log. The second, Stop, obviously
represents the message requesting that the logging thread terminate, but it
contains a field of type MVar (). This enables the sender of the Stop message
to wait for a reply from the logging thread that indicates it has finished.
-}

module Logger where

import Control.Concurrent

data Logger = Logger (MVar LogCommand)

data LogCommand = Message String | Stop (MVar ())

-- | the logger service
logger :: Logger -> IO ()
logger (Logger m) = loop where
  loop = do
    cmd <- takeMVar m
    case cmd of
      Message msg -> do
        putStrLn msg
        loop
      Stop s -> do
        putStrLn "logger: stop"
        putMVar s ()

-- | create/init new logger (that will run in its own thread)
initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

-- | log a/the given message (with the given logger)
logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

-- | stop the (given) logger
logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s
