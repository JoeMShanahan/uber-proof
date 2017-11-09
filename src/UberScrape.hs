{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module UberScrape
  ( getTrips

  -- * Output
  , TripScrapeResult (..)
  , TripRetrievalFailure (..)

  -- * For tests
  , yearAndMonths
  ) where

import           Data.Aeson                   (Value)
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.HashSet                 as HS
import           Data.Text                    (stripPrefix)
import           Data.Time
import           SeleniumUtils
import Data.Attoparsec.Text
import           Test.WebDriver
import           Test.WebDriver.Commands.Wait
import           Types.Expenses
import           Types.Uber
import           Uberlude
import           Vision.Image                 hiding (map)
import           Vision.Image.Storage.DevIL   (Autodetect (..), PNG (..),
                                               loadBS, saveBS)
import           Vision.Primitive             (Rect (..))

data TripScrapeResult = TripScrapeResult
  { tripsRetrieved :: [UberTrip]
  , tripsFailed    :: [TripRetrievalFailure]
  , tripMonth      :: Month
  , tripYear       :: Year
  } deriving (Show)

data TripRetrievalFailure = TripRetrievalFailure TripId SomeException
  deriving (Show)

getTrips :: Day -> Day -> String -> Maybe Int -> Username -> Password -> IO [TripScrapeResult]
getTrips start end host port user pwd = runSession (chromeConfig host port) $ do
  openPage uberPage
  performUberLogin user pwd
  let monthPairs = yearAndMonths start end
      trips (y, m) = tripsInMonth y m
  mapM trips monthPairs

tripsInMonth :: Year -> Month -> WD TripScrapeResult
tripsInMonth y@(Year yy) m@(Month mm) = do
  openPage $ filterTripsURL y m
  putText "Looking for trip ids"
  tripIds <- traverseTableWith $ \n -> filterTripsURLWithPage n y m
  putText $ "Found " <> show (length tripIds) <> " trip(s) for " <> show yy <> "-" <> show mm
  putText "Retrieving trip info"
  eTrips <- forM tripIds $ \tripId -> do
    tripAttempt <- tryWD $ getTripInfo tripId
    case tripAttempt of
      Success trip -> return $ Right trip
      Failure err  -> do
        putText $ "Failed to get trip " <> tripIdText tripId
        putText $ show err
        return $ Left $ TripRetrievalFailure tripId err
  let result = constructResult y m eTrips
  putText $ "Result completed, successful trip retrievals: "
         <> show (length $ tripsRetrieved result)
         <> "/"
         <> show (length tripIds)
  return result

constructResult :: Year -> Month -> [Either TripRetrievalFailure UberTrip] -> TripScrapeResult
constructResult y m = intoType . partitionEithers
  where
  intoType (problems, results) = TripScrapeResult results problems m y

getTripInfo :: TripId -> WD UberTrip
getTripInfo tripId = do
  openPage $ tripPage tripId

  croppedBytes <- takeTripScreenshot

  timeZone <- liftIO getCurrentTimeZone 

  titleEle      <- findElem $ ByClass "page-lead"
  tripStartText <- getText =<< findElemFrom titleEle (ByTag "div")

  utcStart <- case parseUberTime tripStartText of
    Left err   -> unexpected $ "Could not parse time '"<> unpack tripStartText <>"': " <> err
    Right time -> return $ localTimeToUTC timeZone time

  let getAddress e = getText =<< findElemFrom e (ByTag "h6")
  [fromEle, toEle] <- findElems $ ByClass "trip-address"
  fromText         <- getAddress fromEle
  toText           <- getAddress toEle
  arrivalTimeText  <- getText =<< findElemFrom toEle (ByTag "p")

  utcEnd <- case parseUberTimeOfDay arrivalTimeText of
    Left err  -> unexpected $ "Could not parse arrival time '"<> unpack arrivalTimeText <> "': " <> err
    Right tod -> return $ arrivalTimeFromStart utcStart tod


  fareBreakdown <- findElem $ ById "receipt-frame"

  -- iframes - yay
  focusFrame $ WithElement fareBreakdown

  currenciesElems <- findElems $ ByXPath "//div[contains(text(),'£')]"
  currencies      <- forM currenciesElems $ \e -> do
    valueText <- getText e
    currency  <- expectGBP valueText
    return (e, currency)

  (fareEle, farePence) <- case maximumMay $ sortOn snd currencies of
    Just a  -> return a
    Nothing -> unexpected $ "Could not find max value from " <> show currencies 

  let trip = UberTrip { uberTripId     = tripId
                      , uberScreenshot = croppedBytes
                      , uberStartTime  = utcStart
                      , uberEndTime    = utcEnd
                      , uberStartLoc   = fromText
                      , uberEndLoc     = toText
                      , uberCost       = farePence
                      , userCard       = fromMaybe undefined $ makeCard MasterCard 9999
                      }
  return trip

  where
  expectGBP v = case parseOnly parseGBP v of
    Left err -> unexpected $ "Could not parse value " <> show v <> ": " <> err
    Right c  -> return c


arrivalTimeFromStart :: UTCTime -> TimeOfDay -> UTCTime
arrivalTimeFromStart start arriveTimeOfDay =
  if arriveTimeOfDay < timeToTimeOfDay (utctDayTime start)
    then withNewToD { utctDay = succ $ utctDay withNewToD }
    else withNewToD
  where
  withNewToD = start { utctDayTime = timeOfDayToTime arriveTimeOfDay }

takeTripScreenshot :: WD ByteString
takeTripScreenshot = do
  -- Get rid of the trip bar buttons
  void (executeJS [] deleteTripBarJS :: WD Value)

  screenshotBytes       <- screenshot
  tripDetails           <- findElem $ ByClass tripDetailsDivClass
  (eleX, eleY)          <- elemPos tripDetails
  (eleWidth, eleHeight) <- elemSize tripDetails

  let rect = Rect { rX = eleX
                  , rY = eleY
                  , rWidth  = fromIntegral eleWidth
                  , rHeight = fromIntegral eleHeight
                  }

  image <- case loadBS Autodetect $ BSL.toStrict screenshotBytes of
    Left err  -> unexpected $ "Failed to process image: " <> show err
    Right img -> return (img :: RGB)

  let cropped = crop rect image :: RGB

  case saveBS PNG cropped of
    Left err    -> unexpected $ "Failed to convert image to bytes: " <> show err
    Right bytes -> return bytes

traverseTableWith :: (Int -> String) -> WD [TripId]
traverseTableWith mkUrl = go 1
  where
  go n = do
    openPage $ mkUrl n
    ids <- getTripIdsFromTable
    case ids of
      [] -> return ids
      _  -> (ids <>) <$> go (n + 1)

getTripIdsFromTable :: WD [TripId]
getTripIdsFromTable = do
  table     <- findElem $ ById "trips-table"
  tableBody <- findElemFrom table $ ByTag "tbody"
  rows      <- findElemsFrom tableBody $ ByCSS "tr.trip-expand__origin"
  idValues  <- mapM (\e -> attr e "data-target") rows

  let tripIds = mapMaybe getTripId $ catMaybes idValues

  unless (length rows == length tripIds) $ unexpected "Unparsable trip id met"

  return tripIds
  where
  getTripId t = tripIdFromText =<< stripPrefix "#trip-" t

chromeConfig :: String -> Maybe Int -> WDConfig
chromeConfig host port = addPort $ config { wdHost = host }
  where
  config = useBrowser chrome defaultConfig
  addPort = maybe identity (\port' config' -> config' { wdPort = port' }) port

performUberLogin :: Username -> Password -> WD ()
performUberLogin (Username user) (Password pwd) = do
  putText "Attempting login"
  attempt <- tryWD loginProcedure
  case attempt of
    Success _ -> putText "Login successful"
    Failure e -> unexpected $ "Login failed! Reason:\n" <> show e

  where
  loginProcedure = do
    patiently $ enterInput user userInputId "Next"
    patiently $ enterInput pwd passwordInputId "Next"
    let badPwd = do
          e <- findElem $ containsTextSelector "entered is incorrect"
          unlessM (isDisplayed e) $ unexpected "Bad password text exists but not visible"
    patiently (tryEither badPwd loginSuccess) >>= \r -> case r of
      Left _  -> fail "Bad password"
      Right _ -> return ()

  enterInput keys elemId buttonText = do
    input <- findElem $ ById elemId
    clearInput input
    sendKeys keys input
    findAndClick buttonText

  findAndClick t = do
    es <- findElems (containsTextSelector t)
    next <- filterM isDisplayed es
    case next of
      [nxt] -> click nxt
      _     -> unexpected $ unpack $
        "Found " <> show (length es) <> " " <> t <> " buttons"

loginSuccess :: WD ()
loginSuccess = void $ findElem $ containsTextSelector "MY TRIPS"


yearAndMonths :: Day -> Day -> [(Year, Month)]
yearAndMonths start end
  | start <= end = map mkTuple $ uniq $ map yearMonth days
  | otherwise    = []
  where
  days = end : [start, addDays 28 start .. end]
  mkTuple (y, m) = (Year y, Month m)
  uniq = HS.toList . HS.fromList
  yearMonth = (\(y, m, _) -> (y, m)) . toGregorian

{- URLs -}

uberPage :: String
uberPage = "https://riders.uber.com"

filterTripsURL :: Year -> Month -> String
filterTripsURL (Year year) (Month month) =
  uberPage <> "/trips?month=" <> show year <> "-" <> show month

filterTripsURLWithPage :: Int -> Year -> Month -> String
filterTripsURLWithPage page y m = filterTripsURL y m <> "&page=" <> show page

tripPage :: TripId -> String
tripPage tripId = uberPage <> "/trips/" <> tripIdToString tripId

{- Element keys -}

userInputId :: Text
userInputId = "useridInput"

passwordInputId :: Text
passwordInputId = "password"

tripActionBarId :: Text
tripActionBarId = "trip-details__actions"

tripDetailsDivClass :: Text
tripDetailsDivClass = "page-content"

-- | Ugh!
deleteTripBarJS :: Text
deleteTripBarJS = "var elem = document.getElementById(\""<>tripActionBarId<>"\"); elem.parentNode.removeChild(elem);"
