module Main where

import           Options
import           Uberlude
import           UberScrape

main :: IO ()
main = withOptions $ \opt -> do
  let host      = optSeleniumServerHost opt
      port      = optSeleniumServerPort opt
      user      = optUberUser opt
      password  = optUberPass opt
      start     = optStart opt
      end       = optEnd opt

  result <- getTrips start end host port user password
  forM_ result $ \r -> forM_ (tripsFailed r) $ putText . show
