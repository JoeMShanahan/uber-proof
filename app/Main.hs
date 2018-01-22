{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options
import           Uberlude
import           UberScrape
import           Types.Uber
import qualified Data.ByteString as BS
import Data.Time
import System.Directory
import qualified Data.HashMap.Strict as HM
import Data.Char

main :: IO ()
main = withOptions $ \opt -> do
  let host      = optSeleniumServerHost opt
      port      = optSeleniumServerPort opt
      user      = optUberUser opt
      password  = optUberPass opt
      start     = optStart opt
      end       = optEnd opt

  results <- getTrips start end host port user password

  reportFailures $ concatMap tripsFailed results
  processResults results

reportFailures :: [TripRetrievalFailure] -> IO ()
reportFailures = mapM_ $ putText . describeFailure
    
processResults :: [TripScrapeResult] -> IO ()
processResults results = do
  tripdir <- tripDirectory
  forM_ results $ \result -> 
    forM_ (tripsRetrieved result) $ \trip -> do
      let fullDir =  concat $ intersperse "/"
            [ tripdir
            , "receipt_images"
            , show $ unYear $ tripYear result
            , padWith0 $ show $ unMonth $ tripMonth result
            ]
          tripPath t = fullDir <> "/" <> tripFileName t <> ".png"
      createDirectoryIfMissing True fullDir     
      BS.writeFile (tripPath trip) (uberScreenshot trip)
  where
  padWith0 c@[_] = '0':c
  padWith0 cs    = cs

tripFileName :: UberTrip -> FilePath
tripFileName trip = concat $ intersperse "_"
  [ convertToFilename $ pack $ formatUTC $ uberStartTime trip
  , convertToFilename $ uberStartLoc trip
  , "to"
  , convertToFilename $ uberEndLoc trip
  ]
  where
  formatUTC = formatTime defaultTimeLocale "%FT%T%QZ"

tripDirectory :: IO FilePath
tripDirectory = do
  home <- getHomeDirectory
  pure $ home <> "/.uber-proof"

convertToFilename :: Text -> FilePath
convertToFilename = mapMaybe lookup . unpack
  where
  lookup c = HM.lookup c characterConversion

characterConversion :: HM.HashMap Char Char
characterConversion = HM.fromList $
     zip (map toUpper lowers) lowers
  <> zip numChars numChars
  <> zip lowers lowers
  <> [(' ', '-')]
  <> [('-', '-')]
  where
  numChars = concatMap (unpack . show) [0 .. 9 :: Int]
  lowers   = ['a'..'z']
