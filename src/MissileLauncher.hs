module MissileLauncher
  ( MissileLauncher (..)
  , MoveCommand (..)
  , TimeNanoseconds
  , newMissileLauncher
  , cmdMissileLauncher
  , repeatFor
  )
where

import qualified Data.ByteString as BS
import USB
import Text.Printf
import Data.Vector ((!))
import System.USB
import Control.Concurrent
import IRC

-- The vendor ID of the launcher USB device
vendorId :: Num a => a
vendorId = 0x1130

-- The product ID of the launcher USB device
productId :: Num a => a
productId = 0x0202


-- A handle for the missile launcher
data MissileLauncher = MissileLauncher { _ctx :: Ctx, _dev :: Device }


-- Possible movement commands
data MoveCommand = MoveNone
                 | MoveUp
                 | MoveDown
                 | MoveLeft
                 | MoveRight
                 | MoveUpLeft
                 | MoveUpRight
                 | MoveDownLeft
                 | MoveDownRight

-- A type alias representing time in nanoseconds
type TimeNanoseconds = Int


-- Try connecting to a missile launcher, blocking if none was found
newMissileLauncher :: Bool -> IO MissileLauncher
newMissileLauncher debug = do
  ctx <- newCtx
  if debug then setDebug ctx PrintDebug else return ()
  dev <- waitFindDevice ctx vendorId productId
  return $ MissileLauncher ctx dev

-- Repeats an io action every `every' nanoseconds for a total of `time' time
repeatFor time every act = do
  let iterations = time `div` every
      remainder = time `mod` every
      times = replicate iterations every ++ [remainder]
    in mapM_ (\t -> act >> threadDelay t) times

-- Converts a move command to a 4-element mask (that's how the usb messages
-- work, one byte per direction where 0 = no and 1 = yes)
moveCommandToList MoveNone      = [0,0,0,0]
moveCommandToList MoveUp        = [0,0,1,0]
moveCommandToList MoveDown      = [0,0,0,1]
moveCommandToList MoveLeft      = [1,0,0,0]
moveCommandToList MoveRight     = [0,1,0,0]
moveCommandToList MoveUpLeft    = [1,0,1,0]
moveCommandToList MoveUpRight   = [0,1,1,0]
moveCommandToList MoveDownLeft  = [1,0,0,1]
moveCommandToList MoveDownRight = [0,1,0,1]

-- Send a command to a missile launcher
cmdMissileLauncher :: MissileLauncher -> MoveCommand -> Bool -> Maybe TimeNanoseconds -> IO ()
cmdMissileLauncher (MissileLauncher ctx dev) moveCmd fire time = do
  withDeviceHandle dev $ \deviceHandle -> do
    withDetachedKernelDriver deviceHandle 0 $ do
      withClaimedInterface deviceHandle 0 $ do

        -- Control setup for 8 byte packets (two fixed initialiser packets)
        let controlSetup8  = ControlSetup Class
                                          ToInterface
                                          9 0x2 0x1

        -- Control setup for 64 byte packets (the actual do something packet)
        let controlSetup64 = ControlSetup Class
                                          ToInterface
                                          9 0x2 0x0
        -- Build command from options
        -- Command is 8 bytes in the form [0, L, R, U, D, F, 8, 8] where LRUD = directions and F = fire
        -- These bytes should be one or zero
        let command = 0 : moveCommandToList moveCmd ++ (if fire then [1] else [0]) ++ [8, 8]

        -- Fixed initiator packets
        let header = do
              writeControlExact deviceHandle controlSetup8  (BS.pack ([85, 83, 66, 67, 0, 0, 4, 0])) 0
              writeControlExact deviceHandle controlSetup8  (BS.pack ([85, 83, 66, 67, 0, 64, 2, 0])) 0

        let packetMove = do
              header
              -- The control packet
              writeControlExact deviceHandle controlSetup64 (BS.pack (command ++ replicate 56 0)) 0

        let packetStop = do
              header
              -- The stop packet
              writeControlExact deviceHandle controlSetup64 (BS.pack ([0, 0, 0, 0, 0, 0, 8, 8] ++ replicate 56 0)) 0

        packetMove
        putStrLn "packetMove"

        case time of
          Just t -> do
            repeatFor ((min t 8000) * 1000) 500000 packetMove
            packetStop
          _      -> packetMove >> packetStop

    return ()
