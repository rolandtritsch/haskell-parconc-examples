{-# LANGUAGE BangPatterns #-}

{-|
The program takes a list of filenames on the command line and counts
the number of lines in each file, ignoring files that do not exist.
-}

module Chapter09CatchMask where

import Control.Exception (try, getMaskingState, throwIO)
import System.Environment (getArgs)
import System.IO.Error (isDoesNotExistError)
import System.IO (openFile, IOMode(ReadMode), hGetContents)

loop :: Int -> [String] -> IO Int
loop !n [] = return n
loop !n (f:fs) = do
  getMaskingState >>= print
  r <- try (openFile f ReadMode)
  case r of
    Left e
      | isDoesNotExistError e -> loop n fs
      | otherwise -> throwIO e
    Right h -> do
      s <- hGetContents h
      loop (n + length (lines s)) fs

main :: IO ()
main = do
  fs <- getArgs
  n <- loop 0 fs
  print n
