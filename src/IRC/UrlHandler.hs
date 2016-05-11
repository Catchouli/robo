module IRC.UrlHandler where

import IRC
import MissileLauncher
import Text.ParserCombinators.Parsec
import Network.Curl
import Text.Regex
import Control.Monad

urlHandler = defaultHandler
  { _onMessage = onMessage }

-- The message handler
onMessage :: IRCConnection -> String -> String -> String -> IO ()
onMessage conn chan nick msg = do
  let url = matchRegex (mkRegex "(https?://[^ \r\n$]+)") msg
  case url of
    Nothing     -> return ()
    Just [url]  -> do (code, text) <- curlGetString url [CurlFollowLocation True]
                      let title = matchRegex (mkRegex "<[tT][iI][tT][lL][eE].*?>\\s*(.*?)\\s*<\\/[tT][iI][tT][lL][eE]") text
                      case title of
                        Nothing      -> return ()
                        Just [title] -> sendMessage conn chan title
