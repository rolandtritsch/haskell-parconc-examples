{-|
A PhoneBook is a mapping from names to phone numbers represented
by Haskell’s Map type from the Data.Map library. To make this into
a piece of shared mutable state, all we need to do is wrap it in
an MVar. Here, we have made a new type called PhoneBookState to
contain the MVar. This is simply good practice. If we were to
make this interface into a library, the PhoneBookState type could
be exported abstractly so that clients could not see or depend on
its implementation.
-}

module PhoneBook where

import Control.Concurrent
import qualified Data.Map as M

type Name        = String
type PhoneNumber = String
type PhoneBook   = M.Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

-- | new phonebook (state)
new :: IO PhoneBookState
new = do
  m <- newMVar M.empty
  return (PhoneBookState m)

-- | insert/add an entry into the (given) phonebook
insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
  book <- takeMVar m
  putMVar m (M.insert name number book)

-- | lookup an entry from the phonebook
lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return (M.lookup name book)
