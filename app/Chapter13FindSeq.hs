{-|
A simple program that searches the filesystem for files
with a particular name. The program takes a filename to
search for and the root directory for the search as arguments,
and prints either Just p if the file was found with pathname
p or Nothing if it was not found.
-}

module Chapter13FindSeq where

import System.Environment (getArgs)
import Data.List (sort)
import System.FilePath ((</>))
import System.Directory (getDirectoryContents, doesDirectoryExist)

find :: String -> FilePath -> IO (Maybe FilePath)
find s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".",".."]) fs
  if any (== s) fs'
    then return (Just (d </> s))
    else loop fs'
 where
  loop [] = return Nothing
  loop (f:fs)  = do
    let d' = d </> f
    isdir <- doesDirectoryExist d'
    if isdir
      then do
        r <- find s d'
        case r of
          Just _  -> return r
          Nothing -> loop fs
      else loop fs

main :: IO ()
main = do
  [s,d] <- getArgs
  r <- find s d
  print r
