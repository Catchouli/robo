module IRC.Parser where

import Data.ByteString
import Data.Attoparsec.ByteString.Char8

type Nick = String
type Channel = String
type Message = String

data IRCMessage = Welcome
                | Message Channel Nick Message

parseWelcome :: Parser IRCMessage
parseWelcome = do
  --code <- number
  return Welcome

parseMessage :: ByteString -> Maybe IRCMessage
parseMessage s = maybeResult $ parse parseWelcome s
