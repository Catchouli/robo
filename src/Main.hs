module Main where

import IRC
import Network
import Control.Concurrent
import Text.ParserCombinators.Parsec

config :: IRCConfig
config = defaultConfig
  --{ _hostname = "192.168.0.77"
  { _hostname = "uiharu.cat.bio"
  , _port = PortNumber 6667
  , _nick = "robo"
  , _onConnect = onConnect
  , _onMessage = onMessage
  }

onConnect :: IRCConnection -> IO ()
onConnect conn = do
  sendCommand conn "JOIN #rena"

onMessage :: IRCConnection -> String -> String -> String -> IO ()
onMessage conn chan nick msg = do
  let mynick = _nick . _config $ conn
  case parse (string $ "hello " ++ mynick) "" msg of
    Right _    -> sendMessage conn chan $ "hello " ++ nick
    _          -> return ()

main :: IO ()
main = do
  conn <- newConnection config
  forkIO $ process conn
  let loop = do
        a <- getLine
        sendMessage conn "#rena" a
        loop
  loop
  sendCommand conn "QUIT"
