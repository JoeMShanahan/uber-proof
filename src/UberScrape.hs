module UberScrape
  ( getTrips

  -- * For tests
  , yearAndMonths
  ) where

import qualified Data.HashSet as HS
import Data.Text (toLower, isInfixOf, stripPrefix)
import           Data.Time
import Uberlude hiding (withAsync)
import Types.Uber
import Test.WebDriver
import Test.WebDriver.Commands.Wait (waitUntil, onTimeout, unexpected)
import Control.Concurrent.Async.Lifted

getTrips :: Day -> Day -> String -> Maybe Int -> Username -> Password -> IO [UberTrip]
getTrips start end host port user pwd = runSession (chromeConfig host port) $ do
  openPage uberPage
  performUberLogin user pwd
  let months = yearAndMonths start end
      trips (y, m) = tripsInMonth y m
  concat <$> mapM trips months

tripsInMonth :: Year -> Month -> WD [UberTrip]
tripsInMonth y m = do
  openPage $ filterTripsURL y m
  liftIO $ threadDelay 10000000000000
  undefined

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
    Failure   -> unexpected "Login failed!"

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

containsTextSelector :: Text -> Selector
containsTextSelector t = ByXPath $
     "//*[text()[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'),'"
  <> toLower t
  <> "')]]"

patiently :: WD a -> WD a
patiently = waitUntil waitTime

waitTime :: Double
waitTime = 60

data WDSuccess a = Success a | Failure
  deriving (Eq, Show)

tryWD :: WD a -> WD (WDSuccess a)
tryWD go = waitUntil 0 (go >>= return . Success) `onTimeout` return Failure

tryEither :: WD a -> WD b -> WD (Either a b)
tryEither getA getB = do
  a <- tryWD getA
  case a of
    Success s -> return $ Left s
    Failure   -> Right <$> getB

uberPage :: String
uberPage = "https://riders.uber.com"

filterTripsURL :: Year -> Month -> String
filterTripsURL (Year year) (Month month) =
  uberPage <> "/trips?month=" <> show year <> "-" <> show month

captchaSuccessClass :: Text
captchaSuccessClass = "recaptcha-checkbox-checkmark"

userInputId :: Text
userInputId = "useridInput"

passwordInputId :: Text
passwordInputId = "password"

mfaInputId :: Text
mfaInputId = "verificationCode"

tryUntil :: WD a -> WD a -> WD a
tryUntil attempt until = withAsync attempt $ const untilLoop
  where
  untilLoop = go =<< tryWD until
  go result = case result of 
    Success a -> return a
    Failure   -> untilLoop

yearAndMonths :: Day -> Day -> [(Year, Month)]
yearAndMonths start end
  | start <= end = map mkTuple $ uniq $ map yearMonth days
  | otherwise    = []
  where
  days = end : [start, addDays 28 start .. end]
  mkTuple (y, m) = (Year y, Month m)
  uniq = HS.toList . HS.fromList
  yearMonth = (\(y, m, _) -> (y, m)) . toGregorian

uberTripClassToId :: Text -> Maybe Text
uberTripClassToId = stripPrefix "trips-"