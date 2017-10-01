module Main where

import Options
import Uberlude
import UberScrape

main :: IO ()
main = withOptions $ \opt -> do
  let host = optSeleniumServerHost opt
      port = optSeleniumServerPort opt
      user = optUberUser opt
      pass = optUberPass opt
  void $ getTrips host port user pass