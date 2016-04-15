module USB
  ( waitFindDevice
  , deviceInfo
  )
where

import System.USB
import Control.Concurrent
import Text.Printf
import qualified Data.Vector as V ( toList )

-- Attempts to find a device, blocking if hotplugging is available
waitFindDevice :: Ctx -> VendorId -> ProductId -> IO Device
waitFindDevice = waitForDevice

-- Block waiting for a device to be hotplugged
waitForDevice :: Ctx -> VendorId -> ProductId -> IO Device
waitForDevice ctx vid pid = do
  mv <- newEmptyMVar
  h <- registerHotplugCallback ctx
                               deviceArrived
                               enumerate
                               (Just vid)
                               (Just pid)
                               Nothing
                               (\dev event -> do tryPutMVar mv (dev, event)
                                                 return DeregisterThisCallback)
  (dev, _event) <- takeMVar mv
  return dev

-- Find a connected device
findDevice :: Ctx -> VendorId -> ProductId -> IO Device
findDevice ctx vid pid = undefined

-- Get device info
deviceInfo :: Device -> [String]
deviceInfo dev =
  [ printf "deviceSpeed:   %s" (maybe "-" show $ deviceSpeed dev)
  , printf "busNumber:     %s" (show $ busNumber dev)
  , printf "portNumber:    %s" (show $ portNumber dev)
  , printf "portNumbers:   %s" (maybe "-" (show . V.toList) $
                                  portNumbers dev 7)
  , printf "deviceAddress: %s" (show $ deviceAddress dev)
  ]
