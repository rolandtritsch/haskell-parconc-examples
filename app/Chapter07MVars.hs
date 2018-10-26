{-|
The MVar is empty when it is created, the child thread puts the
value x into it, and the main thread takes the value and prints
it. And again with/for 'y'.

If the main thread calls takeMVar before the child thread has
put the value, no problem: takeMVar blocks until the value is
available.

The output when we run the program will be 'x' followed by 'y'.
An MVar can be used in this way as a simple channel between two
threads, or even between many writers and a single reader.
-}

module Chapter07MVars where

import Control.Concurrent

main :: IO ()
main = do
  m <- newEmptyMVar
  forkIO $ do
    putMVar m 'X'
    putMVar m 'Y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
