{-|
Imagine a window manager that manages multiple desktops. The user can move windows
from one desktop to another, while at the same time, a program can request that its
own window move from its current desktop to another desktop. The window manager uses
multiple threads: one to listen for input from the user, a set of threads to listen
for requests from the programs running in each existing window, and one thread that
renders the display to the user.

A display consists of a number of Desktops, each of which is displaying a set of
Windows. To put it another way, a display is a mapping from Desktop to a set of
Window objects. The mapping changes over time, so we want to make it mutable, and
the state needs to be shared among multiple threads.
-}

module Chapter10WindowMan where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent.STM

data Desktop = Desktop deriving (Eq, Ord)
data Window = Window deriving (Eq, Ord)

type Display = M.Map Desktop (TVar (S.Set Window))
type UserFocus = TVar Desktop

moveWindowSTM :: Display -> Window -> Desktop -> Desktop -> STM ()
moveWindowSTM disp win a b = do
  wa <- readTVar ma
  wb <- readTVar mb
  writeTVar ma (S.delete win wa)
  writeTVar mb (S.insert win wb)
 where
  ma = disp M.! a
  mb = disp M.! b

moveWindow :: Display
           -> Window -> Desktop -> Desktop
           -> IO ()
moveWindow disp win a b = atomically $ moveWindowSTM disp win a b

swapWindows :: Display
            -> Window -> Desktop
            -> Window -> Desktop
            -> IO ()
swapWindows disp w a v b = atomically $ do
  moveWindowSTM disp w a b
  moveWindowSTM disp v b a

render :: S.Set Window -> IO ()
render = undefined

getWindows :: Display -> UserFocus -> STM (S.Set Window)
getWindows disp focus = do
  desktop <- readTVar focus
  readTVar (disp M.! desktop)

renderThread :: Display -> UserFocus -> IO ()
renderThread disp focus = do
  wins <- atomically $ getWindows disp focus
  loop wins
 where
  loop wins = do
    render wins
    next <- atomically $ do
      wins' <- getWindows disp focus
      if (wins == wins') then retry
      else return wins'
    loop next

main :: IO ()
main = putStrLn "Hello World"
