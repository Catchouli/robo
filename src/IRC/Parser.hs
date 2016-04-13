module IRC.Parser
  ( Nick
  , Username
  , User (..)
  , Channel
  , Message
  , Hostname
  , IRCMessage (..)
  , parseMessage
  )
  where

import Text.ParserCombinators.Parsec

type Nick = String
type Username = String
type Channel = String
type Message = String
type Hostname = String

data User = User Nick Username Hostname deriving Show

data IRCMessage = Welcome String Message
                | Ping Hostname
                | Message Channel User Message
                deriving Show

hostname = many (alphaNum <|> char '.')

parseWelcome = do
  char ':'
  host <- hostname
  space
  code <- string "001"
  space >> manyTill anyToken space >> char ':'
  message <- many anyToken
  return $ Welcome host message

parsePing = do
  string "PING :"
  server <- many anyToken
  return $ Ping server

parsePrivmsg = do
  char ':'
  nick <- manyTill anyToken (char '!')
  user <- manyTill anyToken (char '@')
  host <- manyTill anyToken space
  string "PRIVMSG "
  chan <- manyTill anyToken space
  char ':'
  message <- many anyToken
  return $ Message chan (User nick user host) message

messageParser = try parseWelcome
            <|> try parsePing
            <|> try parsePrivmsg

parseMessage :: String -> Either ParseError IRCMessage
parseMessage s = parse messageParser "irc message" s
