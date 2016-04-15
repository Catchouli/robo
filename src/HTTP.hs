{-# LANGUAGE OverloadedStrings #-}

module HTTP (unsafePerformHttp, unsafePerformAuthedHttp) where

import Network.HTTP.Conduit
import Network.Connection
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as BS

-- http://stackoverflow.com/a/21310691

-- | Get a new Manager that doesn't verify SSL certificates
noSSLVerifyManager :: IO Manager
noSSLVerifyManager = let tlsSettings = TLSSettingsSimple {
                            -- This is where we disable certificate verification
                            settingDisableCertificateValidation = True,
                            settingDisableSession=False,
                            settingUseServerName=True}
                     in newManager $ mkManagerSettings tlsSettings Nothing

-- | Download like with simpleHttp, but using an existing manager for the task
simpleHttpWithManager :: Manager -> String -> IO String
simpleHttpWithManager manager url = do url' <- parseUrl url
                                       fmap (LB.unpack . responseBody) $ httpLbs url' manager

simpleAuthedHttpWithManager :: Manager -> String -> String -> String -> IO String
simpleAuthedHttpWithManager manager url user pass = do url' <- parseUrl url
                                                       let request = applyBasicAuth (BS.pack user) (BS.pack pass) url'
                                                       fmap (LB.unpack . responseBody) $ httpLbs request manager

unsafePerformHttp url = do
  manager <- noSSLVerifyManager
  content <- simpleHttpWithManager manager url
  return content

unsafePerformAuthedHttp url user pass = do
  manager <- noSSLVerifyManager
  return $ simpleAuthedHttpWithManager manager url user pass
