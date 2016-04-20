module Main where

import IRC
import MissileLauncher
import Network
import Control.Concurrent
import Text.ParserCombinators.Parsec
import Control.Concurrent

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Exception

import Data.Typeable

import Data.IORef

config :: IRCConfig
config = defaultConfig
  --{ _hostname = "192.168.0.77"
  { _hostname = "uiharu.cat.bio"
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


simpleMissile moveCommand fire time = do
  launcher <- newMissileLauncher False
  cmdMissileLauncher launcher moveCommand fire time


ifRight :: Monad m => Either a b -> (b -> m ()) -> m ()
ifRight (Left _) _ = return ()
ifRight (Right b) f = f b


onMessage :: IRCConnection -> String -> String -> String -> IO ()
onMessage conn chan nick msg = do
  let mynick = _nick . _config $ conn

  -- Handle missile actions
  let action = choice $ string <$> ["left", "right", "up", "down", "fire"]
      parser = do
        string mynick >> space
        command <- action
        space
        time <- optionMaybe . many1 $ digit
        return (command, read <$> time)
      result (action, time) = simpleMissile (moveCmd action) (action == "fire") time
    in ifRight (parse parser "" msg) result


bot :: IO ()
bot = do
  conn <- newConnection config
  process conn


main :: IO ()
main = do
  --botExited <- newEmptyMVar :: IO (MVar ())
  --forkFinally bot (\_ -> putMVar botExited ())
  
  --launcher <- newMissileLauncher True
  --cmdMissileLauncher launcher MoveNone True Nothing--(Just 1000000)

  --takeMVar botExited

  return ()
