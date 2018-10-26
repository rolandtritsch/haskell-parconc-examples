{-|
Simple main function that inserts a few entries in a phone book
and then does a couple of lookups.
-}

module Chapter07PhoneBook where

import qualified PhoneBook as P

main :: IO ()
main = do
  s <- P.new
  sequence_ [ P.insert s ("name" ++ show n) (show n) | n <- [1..10000] ]
  P.lookup s "name999" >>= print
  P.lookup s "unknown" >>= print
