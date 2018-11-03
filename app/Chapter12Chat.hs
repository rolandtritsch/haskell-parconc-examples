{-# LANGUAGE RecordWildCards #-}

{-|
A network chat server. A chat server enables multiple clients to connect
and type messages to one another interactively. Real chat servers (e.g. IRC)
have multiple channels and allow clients to choose which channels to participate
in. For simplicity, we will be building a chat server that has a single channel,
whereby every message is seen by every client.

The informal specification for the server is as follows:

When a client connects, the server requests the name that the client will be
using. The client must choose a name that is not currently in use; otherwise,
the server will request that the user choose a different name.

Each line received from the client is interpreted as a command, which is one
of the following:

/tell name message - Sends message to the user name.
/kick name Disconnects user name.
/quit Disconnects the current client.
message

Any other string (not beginning with /) is broadcast as a message to all the
connected clients. Whenever a client connects or disconnects, all other connected
clients are notified. We will be handling errors correctly and aiming for consistent
behavior. For example, when two clients connect at the same time, one of them is
always deemed to have connected first and gets notified about the other client
connecting. If two clients simultaneously try to kick each other, only one of
them will succeed.
-}

module Chapter12Chat where

import Text.Printf (printf, hPrintf)
import Network (withSocketsDo, accept, listenOn, PortID(PortNumber))
import Control.Monad (forever, join, when, void)
import Control.Exception (mask, finally)
import System.IO (
  Handle
  , BufferMode(LineBuffering)
  , hClose
  , hPutStrLn
  , hGetLine
  , hSetBuffering
  , hSetNewlineMode
  , universalNewlineMode
  )
import Control.Concurrent (forkFinally, ThreadId, myThreadId, killThread)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (
  STM
  , TVar
  , TChan
  , atomically
  , newTVar
  , newTVarIO
  , readTVar
  , writeTVar
  , modifyTVar
  , newTChan
  , readTChan
  , writeTChan
  )

import qualified Data.Map.Strict as M

type ClientName = String

data Client = Client {
  clientName       :: ClientName
  , clientHandle   :: Handle
  , clientKicked   :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  }

{-|
Where, respectively: Notice is a message from the server, Tell is a private
message from another client, Broadcast is a public message from another
client, and Command is a line of text received from the user (via the
receive thread).
-}
data Message
  = Notice String
  | Tell ClientName String
  | Broadcast ClientName String
  | Command String

data Server = Server {
  clients :: TVar (M.Map ClientName Client)
  , mtid :: ThreadId
  }

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return Client {
    clientName       = name
    , clientHandle   = handle
    , clientSendChan = c
    , clientKicked   = k
    }

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = writeTChan clientSendChan msg

newServer :: ThreadId -> IO Server
newServer mtid = do
  c <- newTVarIO M.empty
  return (Server c mtid)

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (M.elems clientmap)

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if M.member name clientmap then return Nothing
  else do
    client <- newClient name handle
    writeTVar clients $ M.insert name client clientmap
    broadcast server  $ Notice (name ++ " has connected")
    return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar clients $ M.delete name
  broadcast server $ Notice (name ++ " has disconnected")

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  readName
 where
  readName = do
    hPutStrLn handle "What is your name?"
    name <- hGetLine handle
    if null name then readName
    else mask $ \restore -> do
      ok <- checkAddClient server name handle
      case ok of
        Nothing -> restore $ do
          hPrintf handle "The name %s is in use, please choose another\n" name
          readName
        Just client -> restore  $ finally (runClient server client) (removeClient server name)

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  race server receive
  return ()
 where
  receive = forever $ do
    msg <- hGetLine clientHandle
    atomically $ sendMessage client (Command msg)

  server = join $ atomically $ do
    k <- readTVar clientKicked
    case k of
      Just reason -> return $
        hPutStrLn clientHandle $ "You have been kicked: " ++ reason
      Nothing -> do
        msg <- readTChan clientSendChan
        return $ do
          continue <- handleMessage serv client msg
          when continue $ server

sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
  clientmap <- readTVar clients
  case M.lookup name clientmap of
    Nothing -> return False
    Just client -> sendMessage client msg >> return True

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server{..} Client{..} who msg = do
  ok <- atomically $ sendToName server who (Tell clientName msg)
  if ok then return ()
  else hPutStrLn clientHandle (who ++ " is not connected.")

kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} who by = do
  clientmap <- readTVar clients
  case M.lookup who clientmap of
    Nothing ->
      void $ sendToName server by (Notice $ who ++ " is not connected")
    Just victim -> do
      writeTVar (clientKicked victim) $ Just ("by " ++ by)
      void $ sendToName server by (Notice $ "you kicked " ++ who)

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message = case message of
  Notice msg         -> output $ "*** " ++ msg
  Tell name msg      -> output $ "*" ++ name ++ "*: " ++ msg
  Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
  Command msg -> case words msg of
    ["/kick", who] -> do
      atomically $ kick server who clientName
      return True
    "/tell" : who : what -> do
      tell server client who (unwords what)
      return True
    ["/end"] -> do
      hPutStrLn clientHandle "End the session ..."
      return False
    ["/quit"] -> do
      hPutStrLn clientHandle "Shutdown the server ..."
      killThread $ mtid server
      return False
    ('/':_):_ -> do
      hPutStrLn clientHandle $ "Unrecognized command: " ++ msg
      return True
    _ -> do
      atomically $ broadcast server $ Broadcast clientName msg
      return True
 where
   output s = do
     hPutStrLn clientHandle s
     return True

port :: Int
port = 44444

main :: IO ()
main = withSocketsDo $ do
  mainThreadId <- myThreadId
  server <- newServer mainThreadId
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d ...\n" port
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s:%s ...\n" host (show port)
      forkFinally (talk handle server) (\_ -> hClose handle)
