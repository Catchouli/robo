module IRC
  ( IRCConfig (..)
  , Nick
  , Channel
  , Message
  , defaultConfig
  , IRCConnection (..)
  , newConnection
  , process
  , sendCommand
  , sendMessage
  , IRCHandler (..)
  , defaultHandler
  , ifRight
  )
  where

import Network
import System.IO
import Text.Printf
import Control.Concurrent
import IRC.Parser

-- Structure representing an IRC config - basically one client
data IRCConfig = IRCConfig
  { _hostname :: String
  , _port :: PortID
  , _nick :: Nick
  , _backup :: Nick
  , _handlers :: [IRCHandler]
  }

-- The default do-nothing config
defaultConfig :: IRCConfig
defaultConfig = IRCConfig
  { _hostname = ""
  , _port = PortNumber 6667
  , _nick = ""
  , _backup = ""
  , _handlers = []
  }

-- Data representing an event handler, irc configs can have multiple of
-- these to allow behaviour to be snapped in
data IRCHandler = IRCHandler
  { _onConnect :: (IRCConnection -> IO ())
  , _onMessage :: (IRCConnection -> Channel -> Nick -> Message -> IO ())
  }

-- The default do-nothing handler
defaultHandler :: IRCHandler
defaultHandler = IRCHandler
  { _onConnect = \_ -> return ()
  , _onMessage = \_ _ _ _ -> return ()
  }

-- An irc connection handle
data IRCConnection = IRCConnection
  { _config :: IRCConfig
  , _handle :: Handle
  }

-- Send an onConnect event to all of a connection's handlers
onConnect :: IRCConnection -> IO ()
onConnect conn = mapM_ (\h -> _onConnect h conn) (_handlers . _config $ conn)

-- Send an onMessage event to all of a connection's handlers
onMessage :: IRCConnection -> Channel -> Nick -> Message -> IO ()
onMessage conn x y z = mapM_ (\h -> _onMessage h conn x y z) (_handlers . _config $ conn)

-- Create a new IRC connection based on a config
newConnection :: IRCConfig -> IO IRCConnection
newConnection conf = do
  h <- connectTo (_hostname conf) (_port conf)

  let conn = IRCConnection { _config = conf
                           , _handle = h }

  let nick = _nick conf

  sendCommand conn $ "NICK " ++ nick
  sendCommand conn $ "USER " ++ nick ++ " 0 * :" ++ nick

  return conn

-- Process an irc connection, blocking (use forkIO to make not blocking)
process :: IRCConnection -> IO ()
process conn = do
  s <- hGetLine (_handle conn)
  putStrLn s
  processCommand conn s
  process conn

-- Processes a single IRC message
processCommand :: IRCConnection -> String -> IO ()
processCommand conn s = do
  case parseMessage s of
    Right (Welcome host msg)                 -> onConnect conn
    Right (Ping host)                        -> sendCommand conn $ "PONG :" ++ host
    Right (Message chan (User nick _ _) msg) -> onMessage conn chan nick msg
    Right (NickInUse)                        -> sendCommand conn $ "NICK " ++ (_backup . _config $ conn)
    _                                        -> return () --printf "no parse for message\r\n"
  return ()

-- Sends a command to an irc connection
sendCommand :: IRCConnection -> String -> IO ()
sendCommand conn command = do
  hPrintf (_handle conn) "%s\r\n" command
  printf "-> %s\r\n" command

-- Sends a message to an irc connection
sendMessage :: IRCConnection -> Channel -> Message -> IO ()
sendMessage conn channel message = do
  sendCommand conn $ "PRIVMSG " ++ channel ++ " :" ++ message

-- Sends an action to an irc connection
sendAction :: IRCConnection -> Channel -> Message -> IO ()
sendAction conn channel message = sendMessage conn channel $ "\1ACTION " ++ message ++ "\1"

-- If the either value is a right value, evaluate the monadic function at its value
ifRight :: Monad m => Either a b -> (b -> m ()) -> m ()
ifRight (Left _) _ = return ()
ifRight (Right b) f = f b
