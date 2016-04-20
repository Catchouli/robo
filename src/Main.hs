{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}

module Main where

import IRC
import IRC.MissileHandler
import Network

import Text.ParserCombinators.Parsec

import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Exception

import Data.Typeable
import Data.IORef
import Data.Acid
import Data.SafeCopy
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader
import System.Environment

-- The quote database structure
data QuoteDB = QuoteDB (Map.Map String String)
  deriving (Show, Typeable)

-- The acid state stuff for QuoteDB
$(deriveSafeCopy 0 'base ''QuoteDB)
updateQuoteDB :: Map.Map String String -> Update QuoteDB ()
updateQuoteDB newValue = put (QuoteDB newValue)
queryQuoteDB :: Query QuoteDB (Map.Map String String)
queryQuoteDB = do QuoteDB quote <- ask
                  return quote
$(makeAcidic ''QuoteDB ['updateQuoteDB, 'queryQuoteDB])

-- Add a quote to the specified quote db
addQuote qdb key value = do
  map <- query qdb QueryQuoteDB
  update qdb $ UpdateQuoteDB (Map.insert key value map)

-- Get a quote from the specifiec quote db
getQuote qdb key = do
  map <- query qdb QueryQuoteDB
  return $ Map.lookup key map

acid :: IO ()
acid = do
  acid <- openLocalState (QuoteDB (Map.empty))
  return ()

config :: IRCConfig
config = defaultConfig
  --{ _hostname = "192.168.0.77"
  { _hostname = "uiharu.cat.bio"
  , _nick = "robo"
  , _backup = "robo2"
  , _handlers = [ defaultHandler { _onConnect = onConnect }
                , rocketHandler
                ]
  }


onConnect :: IRCConnection -> IO ()
onConnect conn = do
  sendCommand conn "JOIN #test"





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
