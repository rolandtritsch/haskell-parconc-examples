{-|
Main module to run a/the simple logger.
-}

module Chapter07Logger where

import Logger

main :: IO ()
main = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "bye"
  logStop l
