
module SeleniumUtils
  (
  -- * Selectors
    containsTextSelector

  -- * Helpers
  , indefinitely
  , patiently

  -- * Graceful failure
  , WDResult (..)
  , maybeWDResult
  , tryWD
  , tryEither
  , tryUntil
  ) where

import           Control.Concurrent.Async.Lifted
import           Data.Text                       (toLower)
import           Test.WebDriver
import           Test.WebDriver.Commands.Wait    (unexpected, waitUntil')
import           Uberlude                        hiding (withAsync)

containsTextSelector :: Text -> Selector
containsTextSelector t = ByXPath $
     "//*[text()[contains(translate(., '"
  <> uppers
  <> "','"
  <> lowers
  <> "'),'"
  <> toLower t
  <> "')]]"
  where
  lowers = toLower uppers
  uppers = pack ['A'..'Z']

indefinitely :: WD a -> WD a
indefinitely = waitWhilstDoing 10e9 

patiently :: WD a -> WD a
patiently = waitWhilstDoing defaultWaitTime

defaultWaitTime :: Double
defaultWaitTime = 60

waitWhilstDoing :: Double -> WD a -> WD a
waitWhilstDoing waitTime go = waitUntil' 0 waitTime catchEmAll
  where
  -- | I freely admit this is shameful, but this function is supposed to keep trying no matter what.
  catchEmAll = go `catch` superBadCatcher
  superBadCatcher :: SomeException -> WD a
  superBadCatcher e = unexpected $ "I failed: " <> show e

data WDResult a = Success a | Failure SomeException
  deriving (Show)

maybeWDResult :: WDResult a -> Maybe a
maybeWDResult (Success a) = Just a
maybeWDResult (Failure _) = Nothing

tryWD :: WD a -> WD (WDResult a)
tryWD go = (Success <$> go) `catch` (return . Failure)

tryEither :: WD a -> WD b -> WD (Either a b)
tryEither getA getB = do
  a <- tryWD getA
  case a of
    Success s -> return $ Left s
    Failure _ -> Right <$> getB

tryUntil :: WD a -> WD a -> WD a
tryUntil attempt reqSuccess = withAsync attempt $ const untilLoop
  where
  untilLoop = go =<< tryWD reqSuccess
  go result = case result of
    Success a -> return a
    Failure _ -> untilLoop
