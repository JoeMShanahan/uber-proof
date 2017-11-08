module UberScrape
  ( getTrips

  -- * Output
  , TripScrapeResult (..)
  , TripRetrievalFailure (..)

  -- * For tests
  , yearAndMonths
  ) where

import qualified Data.HashSet                 as HS
import           Data.Text                    (stripPrefix)
import Data.Aeson (Value)
import           Data.Time
import           SeleniumUtils
import           Test.WebDriver
import           Test.WebDriver.Commands.Wait
import           Types.Uber
import           Uberlude

data TripScrapeResult = TripScrapeResult
  { tripsRetrieved :: [UberTrip]
  , tripsFailed    :: [TripRetrievalFailure]
  , tripMonth      :: Month
  , tripYear       :: Year
  } deriving (Show)

data TripRetrievalFailure = TripRetrievalFailure TripId FailedCommand
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
  void $ (executeJS [] deleteTripBarJS :: WD Value)
  saveScreenshot $ tripIdToString tripId <> ".png"
  return UberTrip
    { uberTripId     = tripId
    , uberScreenshot = ""
    , uberStartTime  = UTCTime (fromGregorian 2017 01 01) 0
    , uberStartLoc   = "Nowhere"
    , uberEndLoc     = "Somewhere"
    , uberCost       = 0
    , userCard       = undefined
    }

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

-- | Ugh!
deleteTripBarJS :: Text
deleteTripBarJS = "var elem = document.getElementById(\""<>tripActionBarId<>"\"); elem.parentNode.removeChild(elem);"
