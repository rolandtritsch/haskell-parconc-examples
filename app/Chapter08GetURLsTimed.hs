{-|
Download webpages in parallel (and measure the time it takes to downlad them).
-}

module Chapter08GetURLsTimed where

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

timeDownload :: String -> IO ()
timeDownload url = do
  (time, page) <- timeItT $ getURL url
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

main :: IO ()
main = do
  as <- mapM (async . timeDownload) sites
  mapM_ wait as
