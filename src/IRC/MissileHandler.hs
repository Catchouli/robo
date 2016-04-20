module IRC.MissileHandler where

import IRC
import MissileLauncher
import Text.ParserCombinators.Parsec

-- A handler for rocket related messages
rocketHandler = defaultHandler
  { _onMessage = onMessage }

-- Connect to the missile launcher and launch the missile (or do some other action)
simpleMissile moveCommand fire time = do
  launcher <- newMissileLauncher False
  cmdMissileLauncher launcher moveCommand fire time

-- The message handler
onMessage :: IRCConnection -> String -> String -> String -> IO ()
onMessage conn chan nick msg = do
  let mynick = _nick . _config $ conn

  -- Handle missile actions
  let
      -- The parser for a single action
      action = choice $ string <$> ["left", "right", "up", "down", "fire"]
      -- The comand parser
      parser = do
        string mynick >> space
        command <- action
        space
        time <- optionMaybe . many1 $ digit
        return (command, read <$> time)
      -- Convert a verb to a movement command
      moveCmd "up"    = MoveUp
      moveCmd "down"  = MoveDown
      moveCmd "left"  = MoveLeft
      moveCmd "right" = MoveRight
      moveCmd _       = MoveNone
      -- The resulting action
      result (action, time) = simpleMissile (moveCmd action) (action == "fire") time
    in ifRight (parse parser "" msg) result
