{-|
Play with Trace (and ThreadScope).
-}

module Chapter15Trace where

import GHC.Conc (labelThread)
import Debug.Trace (traceEventIO)
import Control.Concurrent (
  myThreadId
  , newEmptyMVar
  , forkIO
  , putMVar
  , takeMVar
  )

main :: IO ()
main = do
  t <- myThreadId
  labelThread t "main"
  m <- newEmptyMVar
  t <- forkIO $ putMVar m 'A'
  labelThread t "a"
  t <- forkIO $ putMVar m 'B'
  labelThread t "b"
  traceEventIO "before takeMVar"
  takeMVar m
  takeMVar m
  return ()
