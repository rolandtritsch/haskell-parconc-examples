{-|
A simple example with two threads.

One that repeatedly prints the letter A, while the other repeatedly prints B.
-}

module Chapter07Fork where

import Control.Concurrent
import Control.Monad
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 100 (putChar 'A'))
  replicateM_ 100 (putChar 'B')
