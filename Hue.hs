{-# OPTIONS -Wall #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Hue (
  HueBridge,
  discoverLocalBridges,
) where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)
import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (status200)

type Hostname = String

-- |Represents a Philips Hue bridge on the network. Wraps a host name.
newtype HueBridge = HueBridge Hostname
  deriving Show

data BridgeResponse = BridgeResponse {
  bridgeRId :: String,
  bridgeRIpAddress :: String,
  bridgeRMacAddress :: String
}

instance FromJSON BridgeResponse where
  parseJSON (Object v) = BridgeResponse <$>
    v .: "id" <*>
    v .: "internalipaddress" <*>
    v .: "macaddress"
  parseJSON _ = mzero

discoverLocalBridges :: IO [HueBridge]
discoverLocalBridges = do
  req <- parseUrl "http://www.meethue.com/api/nupnp"
  withManager $ \manager -> do
    res <- httpLbs req manager
    if responseStatus res == status200 then
      case (decode $ responseBody res) :: Maybe [BridgeResponse] of
          Nothing -> fail "Could not decode response"
          Just bridges ->
            return $ map (HueBridge . bridgeRIpAddress) bridges
    else fail ("Got status code " ++ (show $ responseStatus res))
