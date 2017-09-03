module Main where

import Options
import Uberlude
import Network.URI
import UberScrape

main :: IO ()
main = withOptions $ \opt -> do
  void $ getTrips "" 0