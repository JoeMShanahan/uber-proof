module Main where

import Options
import Uberlude
import UberScrape

main :: IO ()
main = withOptions $ \opt -> do
  void $ getTrips (optSeleniumServerHost opt) (optSeleniumServerPort opt)