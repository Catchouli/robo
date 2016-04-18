module Main where

import IRC
import MissileLauncher
import Network
import Control.Concurrent
import Text.ParserCombinators.Parsec
import Control.Concurrent

config :: IRCConfig
config = defaultConfig
  { _hostname = "192.168.0.77"
  --{ _hostname = "uiharu.cat.bio"
  , _port = PortNumber 6667
  , _nick = "robo"
  , _backup = "robo2"
  , _onConnect = onConnect
  , _onMessage = onMessage
  }


onConnect :: IRCConnection -> IO ()
onConnect conn = do
  sendCommand conn "JOIN #test"


moveCmd "up"    = MoveUp
moveCmd "down"  = MoveDown
moveCmd "left"  = MoveLeft
moveCmd "right" = MoveRight
moveCmd _       = MoveNone


simpleMissile moveCommand fire = do
  launcher <- newMissileLauncher False
  cmdMissileLauncher launcher moveCommand fire Nothing


onMessage :: IRCConnection -> String -> String -> String -> IO ()
onMessage conn chan nick msg = do
  let mynick = _nick . _config $ conn
  case parse ((string $ mynick ++ " ") >> (string "left" <|> string "right" <|> string "up" <|> string "down" <|> string "fire")) "" msg of
    Right cmd  -> simpleMissile (moveCmd cmd) (cmd == "fire")
    Left err   -> return () --print err


bot :: IO ()
bot = do
  conn <- newConnection config
  process conn


main :: IO ()
main = do
  botExited <- newEmptyMVar :: IO (MVar ())
  forkFinally bot (\_ -> putMVar botExited ())
  
  --launcher <- newMissileLauncher True
  --cmdMissileLauncher launcher MoveNone True Nothing--(Just 1000000)

  takeMVar botExited
