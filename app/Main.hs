{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import qualified Data.Text as T
import Types.Expenses

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
  forM_ results $ \result -> do

    let yearText  = show $ unYear $ tripYear result
        monthText = padWith0 $ show $ unMonth $ tripMonth result
        csvDir    = concat $ intersperse "/" [tripdir, "csvs"]
        trips     = tripsRetrieved result

    createDirectoryIfMissing True csvDir

    let csvFilename = csvDir <> "/" <> yearText <> "-" <> monthText <> ".csv"
    BS.writeFile csvFilename $ makeCSVBytes $ sortBy (comparing uberStartTime) trips

    forM_ trips $ \trip -> do
      let imageDir = concat $ intersperse "/"
            [ tripdir
            , "receipt_images"
            , yearText
            , monthText
            ]
          tripPath t = imageDir <> "/" <> tripFileName t <> ".png"
      createDirectoryIfMissing True imageDir     
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

convertTripToCSV :: UberTrip -> TripCSV
convertTripToCSV UberTrip{..} = TripCSV
  { textTime  = timeText uberStartTime
  , textStart = uberStartLoc
  , textEnd   = uberEndLoc
  , textId    = tripIdText uberTripId
  , textCard  = cardText uberCard
  , textCost  = displayCurrencyValue uberCost
  }
  where
  timeText = pack . formatTime defaultTimeLocale excelTimeFormat
  excelTimeFormat = "%Y-%m-%d %H:%M:%S"

tripCSVHeader :: Text
tripCSVHeader = "start time,start location,end location,id,card,cost"

data TripCSV = TripCSV
  { textTime  :: Text
  , textStart :: Text
  , textEnd   :: Text
  , textId    :: Text
  , textCard  :: Text
  , textCost  :: Text
  } deriving (Eq, Show)

tripCSVLine :: TripCSV -> Text
tripCSVLine TripCSV{..} = mconcat $ intersperse "," $ map forceValid
  [ textTime 
  , textStart
  , textEnd  
  , textId   
  , textCard 
  , textCost
  ]
  where
  forceValid = pack . trim . unpack . removeCommas
  removeCommas = T.replace "," " "
  trim (' ':cs) = ' ' : trim (dropWhile (== ' ') cs)
  trim (  c:cs) = c   : trim cs
  trim      []  = []

makeCSVBytes :: [UberTrip] -> ByteString
makeCSVBytes trips = encodeUtf8 $ T.unlines csvLines
  where
  csvLines = tripCSVHeader : map (tripCSVLine . convertTripToCSV) trips
