
module Main where

import IRC
import IRC.MissileHandler
import IRC.QuoteHandler

import Control.Concurrent

config :: IRCConfig
config = defaultConfig
  { _hostname = "192.168.0.77"
  --{ _hostname = "uiharu.cat.bio"
  , _nick = "robo"
  , _backup = "robo2"
  , _handlers = [ defaultHandler { _onConnect = onConnect }
                , rocketHandler
                , quoteHandler
                ]
  }

onConnect :: IRCConnection -> IO ()
onConnect conn = do
  sendCommand conn "JOIN #dev"

bot :: IO ()
bot = do
  conn <- newConnection config
  process conn


main :: IO ()
main = do
  bot
  botExited <- newEmptyMVar :: IO (MVar ())
  forkFinally bot (\_ -> putMVar botExited ())
  
  --launcher <- newMissileLauncher True
  --cmdMissileLauncher launcher MoveNone True Nothing--(Just 1000000)

  takeMVar botExited

  return ()
