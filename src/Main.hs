
module Main where

import IRC
import IRC.MissileHandler
import IRC.QuoteHandler
import IRC.UrlHandler

import Control.Concurrent
import Control.Conditional

import Data.IORef
import Data.List (isInfixOf)

import Mueval.Interpreter
import Language.Haskell.Interpreter (runInterpreter)
import Mueval.ArgsParse
import Mueval.Parallel


bot :: IO (IRCConnection)
bot = do
  newConnection $ defaultConfig { _hostname = "192.168.0.77"
                                , _nick = "testrobo"
                                , _backup = "robo2"
                                , _handlers = [ defaultHandler { _onConnect = onConnect, _onMessage = onMessage2 }
                                              , rocketHandler
                                              , quoteHandler
                                              , urlHandler
                                              ]
                                }


onConnect :: IRCConnection -> IO ()
onConnect conn = do
  sendCommand conn "JOIN #dev"

onMessage2 :: IRCConnection -> Channel -> Nick -> Message -> IO ()
onMessage2 conn chan nick msg = if take 2 msg == "> " then case interpreterOpts ["--expression", tail . tail $ msg] of
                                  Left _ -> return ()
                                  Right a -> do x <- interpreterSession2 a
                                                sendMessage conn chan . take 200 $ x
                                                      else return ()

interpreterSession2 :: Options -> IO (String)
interpreterSession2 opts = do r <- runInterpreter (interpreter opts)
                              case r of
                                Left err -> return . show $ err
                                Right (e,et,val) -> return val

main :: IO ()
main = do
  conn <- bot
  process conn
