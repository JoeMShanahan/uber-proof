module Main where

import Options
import Uberlude
import Network.URI
import SeleniumScrape

main :: IO ()
main = withOptions $ \opt -> do
  void $ getTrips "" 0