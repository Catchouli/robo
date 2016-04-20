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
  )
  where

import Network
import System.IO
import Text.Printf
import Control.Concurrent
import IRC.Parser

data IRCConfig = IRCConfig
  { _hostname :: String
  , _port :: PortID
  , _nick :: Nick
  , _onConnect :: (IRCConnection -> IO ())
  , _onMessage :: (IRCConnection -> Channel -> Nick -> Message -> IO ())
  }

defaultConfig :: IRCConfig
defaultConfig = IRCConfig
  { _hostname = ""
  , _port = PortNumber 6667
  , _nick = ""
  , _onConnect = \_ -> return ()
  , _onMessage = \_ _ _ _ -> return ()
  }

data IRCConnection = IRCConnection
  { _config :: IRCConfig
  , _handle :: Handle
  }

newConnection :: IRCConfig -> IO IRCConnection
newConnection conf = do
  h <- connectTo (_hostname conf) (_port conf)

  let conn = IRCConnection { _config = conf
                           , _handle = h }

  let nick = _nick conf

  sendCommand conn $ "NICK " ++ nick
  sendCommand conn $ "USER " ++ nick ++ " 0 * :" ++ nick

  return conn

process :: IRCConnection -> IO ()
process conn = do
  s <- hGetLine (_handle conn)
  putStrLn s
  processCommand conn s
  process conn

processCommand :: IRCConnection -> String -> IO ()
processCommand conn s = do
  case parseMessage s of
    Right (Welcome host msg)                 -> (_onConnect . _config $ conn) conn
    Right (Ping host)                        -> sendCommand conn $ "PONG :" ++ host
    Right (Message chan (User nick _ _) msg) -> (_onMessage . _config $ conn) conn chan nick msg
    _                                        -> printf "no parse for message\r\n"
  return ()

sendCommand :: IRCConnection -> String -> IO ()
sendCommand conn command = do
  hPrintf (_handle conn) "%s\r\n" command
  printf "-> %s\r\n" command

sendMessage :: IRCConnection -> Channel -> Message -> IO ()
sendMessage conn channel message = do
  sendCommand conn $ "PRIVMSG " ++ channel ++ " :" ++ message

sendAction :: IRCConnection -> Channel -> Message -> IO ()
sendAction conn channel message = sendMessage conn channel $ "\1ACTION " ++ message ++ "\1"
