module Main where

import IRC
import Network

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
  sendCommand conn "JOIN #test"

onMessage :: IRCConnection -> String -> String -> String -> IO ()
onMessage conn chan nick msg = do
  sendMessage conn chan $ "hello " ++ nick

main :: IO ()
main = do
  conn <- newConnection config
  process conn
