{-|
The following program illustrates the creation of threads in a program
that implements timed reminders. The user enters a number of seconds,
and after the specified time has elapsed, the program prints a message
and emits a beep.
-}

module Chapter07Reminders where

import Control.Concurrent
import Text.Printf
import Control.Monad

main :: IO ()
main = forever $ do
  s <- getLine
  forkIO $ setReminder s

setReminder :: String -> IO ()
setReminder s  = do
  let t = read s :: Int
  printf "Ok, I'll remind you in %d seconds\n" t
  threadDelay (10^6 * t)
  printf "%d seconds is up! BING!\BEL\n" t
