module UberScrape
  ( getTrips
  ) where

import Uberlude
import Vision.Image
import Types.UberTrip
import Test.WebDriver

getTrips :: String -> Maybe Int -> IO [UberTrip]
getTrips host port = runSession (chromeConfig host port) $ do
  openPage "http://google.com"
  liftIO $ threadDelay 10000000
  return []

chromeConfig :: String -> Maybe Int -> WDConfig
chromeConfig host port = addPort port $ 
  config { wdHost = host }
  where
  config = useBrowser chrome defaultConfig
  addPort = maybe identity $ \port' config' -> config' { wdPort = port' }