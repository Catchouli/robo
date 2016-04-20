{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}

module IRC.QuoteHandler where

import IRC
import qualified Data.Map as Map
import Data.Typeable
import Data.Acid
import Data.SafeCopy
import Control.Monad.Reader
import Control.Monad.State
import System.IO.Unsafe
import Text.ParserCombinators.Parsec

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

-- A handler for quote db
quoteHandler = defaultHandler
  { _onMessage = onMessage }

-- Handle quote db related commands
onMessage :: IRCConnection -> Channel -> Nick -> Message -> IO ()
onMessage conn chan nick msg = do
  -- Listen for learn commands
  let parser = do string "!learn" >> space
                  key <- manyTill anyToken space
                  value <- many anyToken
                  return (key, value)
      result (key, value) = do
        acid <- openLocalState (QuoteDB (Map.empty))
        addQuote acid key value
        closeAcidState acid
    in ifRight (parse parser "" msg) result 

  -- Listen for requests
  let parser = do string "?" >> space
                  key <- many . noneOf $ " \r\n"
                  return key
      result key = do
        acid <- openLocalState (QuoteDB (Map.empty))
        value <- getQuote acid key
        closeAcidState acid
        case value of
             Just txt -> sendMessage conn chan $ key ++ ": " ++ txt
             Nothing  -> sendMessage conn chan $ "No definition for " ++ key
    in ifRight (parse parser "" msg) result 
