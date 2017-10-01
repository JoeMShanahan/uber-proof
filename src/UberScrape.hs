module UberScrape
  ( getTrips
  ) where

import Uberlude
import Types.Uber
import Test.WebDriver
import Test.WebDriver.Commands.Wait (waitUntil, onTimeout, unexpected)
import Data.Text (toLower)

getTrips :: String -> Maybe Int -> Username -> Password -> IO [UberTrip]
getTrips host port user pwd = runSession (chromeConfig host port) $ do
  openPage uberPage
  performUberLogin user pwd
  return []

chromeConfig :: String -> Maybe Int -> WDConfig
chromeConfig host port = addPort $ config { wdHost = host }
  where
  config = useBrowser chrome defaultConfig
  addPort = maybe identity (\port' config' -> config' { wdPort = port' }) port

performUberLogin :: Username -> Password -> WD ()
performUberLogin (Username user) (Password pwd) = do
  putText "Attempting login"
  
  userInput <- findElem $ ById "useridInput"
  sendKeys user userInput
  findAndClickNext

  passInput <- patiently $ findElem $ ById "password"
  sendKeys pwd passInput
  findAndClickNext

  patiently loggedInOr2FA

  putText "Login successful"

  where
  findAndClickNext = findAndClick "Next"
  findAndClick t = do
    es <- findElems (containsTextSelector t)
    next <- filterM isDisplayed es
    case next of
      [nxt] -> click nxt
      _     -> unexpected $ unpack $
        "Found " <> show (length es) <> " " <> t <> " buttons"

  loggedInOr2FA = do
    login <- tryWD loginSuccess
    case login of
      Success _ -> return ()
      Failure   -> do
        mfaInput <- findElem $ ById "verificationCode"
        putText "It looks like Uber is wanting a 2FA code, please enter it:"
        code <- liftIO getLine
        sendKeys code mfaInput
        findAndClick "Verify"
        patiently loginSuccess


loginSuccess :: WD ()
loginSuccess = void $ findElem $ containsTextSelector "MY TRIPS"

uberPage :: String
uberPage = "https://riders.uber.com"

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
tryWD go = waitUntil 0 (go >>= return . Success) `onTimeout ` return Failure