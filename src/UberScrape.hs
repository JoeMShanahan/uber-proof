module UberScrape
  ( getTrips
  ) where

import Uberlude
import Vision.Image
import Types.Uber
import Test.WebDriver

getTrips :: String -> Maybe Int -> Username -> Password -> IO [UberTrip]
getTrips host port user pass = runSession (chromeConfig host port) $ do
  openPage uberPage
  return []

chromeConfig :: String -> Maybe Int -> WDConfig
chromeConfig host port = addPort $ config { wdHost = host }
  where
  config = useBrowser chrome defaultConfig
  addPort = maybe identity (\port' config' -> config' { wdPort = port' }) port

performUberLogin :: Username -> Password -> WD ()
performUberLogin (Username user) (Password pwd) = do
  undefined

uberPage :: String
uberPage = "https://riders.uber.com"
