{-|
Download webpages in parallel (and and show the first one that finished).
-}

module Chapter08GetURLsFirst where

import Control.Concurrent.Async
import System.TimeIt
import qualified Data.ByteString as B
import Text.Printf (printf)

import GetURL

sites :: [String]
sites = [
  "http://www.google.com",
  "http://www.bing.com",
  "http://www.tritsch.org"
  ]

main :: IO ()
main = do
  let
    download url = do
      r <- getURL url
      return (url, r)

  as <- mapM (async . download) sites

  (_, (url, r)) <- waitAny as

  printf "%s was first (%d bytes)\n" url (B.length r)
  mapM_ wait as
