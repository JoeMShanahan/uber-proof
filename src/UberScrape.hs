module UberScrape
  ( getTrips
  ) where

import Uberlude
import Vision.Image
import Types.UberTrip
import Test.WebDriver

getTrips :: String -> Int -> IO [UberTrip]
getTrips host port = runSession (chromeConfig host port) $ do
  openPage "http://google.com"
  liftIO $ threadDelay 10000000
  undefined

chromeConfig :: String -> Int -> WDConfig
chromeConfig host port = config
  { wdHost = host
  , wdPort = port
  }
  where
  config = useBrowser chrome defaultConfig
