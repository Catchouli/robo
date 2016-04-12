module Main where

import Network
import System.IO
import Text.Printf

server = "192.168.0.77"
port = 6667 :: Num a => a
chan = "#test"
nick = "robo"

main :: IO ()
main = do
  h <- connectTo server (PortNumber port)
  hSetBuffering h NoBuffering
  write h "NICK robo\r\n"
  write h "USER robo 0 * :robo\r\n"
  write h "JOIN #test\r\n"
  let loop = do
      s <- hGetLine h
      putStrLn s
      loop
  loop

write :: Handle -> String -> IO ()
write h s = do
  hPrintf h "%s" s 
  printf "%s" s
