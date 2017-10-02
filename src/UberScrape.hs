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
  attempt <- tryWD loginProcedure
  case attempt of
    Success _ -> putText "Login successful"
    Failure   -> fail "Login failed!"

  where
  loginProcedure = do
    enterInput user userInputId
    enterInput pwd passwordInputId
    patiently loggedInOr2FA

  enterInput keys elemId = do
    input <- findElem $ ById elemId
    sendKeys keys input
    findAndClickNext

  findAndClickNext = findAndClick "Next"
  findAndClick t = do
    es <- findElems (containsTextSelector t)
    next <- filterM isDisplayed es
    case next of
      [nxt] -> click nxt
      _     -> unexpected $ unpack $
        "Found " <> show (length es) <> " " <> t <> " buttons"

  askFor2FA input t = do
    putText $ fromMaybe "It looks like Uber is wanting a 2FA code, please enter it:" t
    code <- liftIO getLine
    clearInput input
    sendKeys code input
    findAndClick "Verify"

  loggedInOr2FA = void $ tryEither loginSuccess process2FA

  process2FA = do
    mfaInput <- findElem $ ById mfaInputId
    askFor2FA mfaInput Nothing
    patiently loginSuccess

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

captchaSuccessClass :: Text
captchaSuccessClass = "recaptcha-checkbox-checkmark"

userInputId :: Text
userInputId = "useridInput"

passwordInputId :: Text
passwordInputId = "password"

mfaInputId :: Text
mfaInputId = "verificationCode"
