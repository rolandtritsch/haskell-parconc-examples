{-|
A simple program that searches the filesystem for files
with a particular name. The program takes a filename to
search for and the root directory for the search as arguments,
and prints either Just p if the file was found with pathname
p or Nothing if it was not found.

Note: This program is build to run on multiple cores.
-}

module Chapter13FindPar where

import System.Environment (getArgs)
import Data.List (sort)
import System.FilePath ((</>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Control.Concurrent.Async (Async, withAsync, wait)

subfind :: String -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        -> [Async (Maybe FilePath)]
        -> IO (Maybe FilePath)
subfind s p inner asyncs = do
  isdir <- doesDirectoryExist p
  if not isdir
     then inner asyncs
     else withAsync (find s p) $ \a -> inner (a:asyncs)

find :: String -> FilePath -> IO (Maybe FilePath)
find s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".",".."]) fs
  if any (== s) fs'
     then return (Just (d </> s))
     else do
       let ps = map (d </>) fs'
       foldr (subfind s) dowait ps []
 where
   dowait as = loop (reverse as)

   loop [] = return Nothing
   loop (a:as) = do
      r <- wait a
      case r of
        Nothing -> loop as
        Just a  -> return (Just a)

main :: IO ()
main = do
  [s,d] <- getArgs
  r <- find s d
  print r
