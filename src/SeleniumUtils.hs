
module SeleniumUtils 
  ( 
  -- * Selectors
    containsTextSelector

  -- * Helpers 
  , patiently

  -- * Graceful failure
  , WDResult (..)
  , tryWD
  , tryEither
  , tryUntil
  ) where

import Uberlude hiding (withAsync)
import Data.Text (toLower)
import Test.WebDriver
import           Test.WebDriver.Commands.Wait    (onTimeout, unexpected,
                                                  waitUntil, waitUntil')
import           Control.Concurrent.Async.Lifted

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
  lowers, uppers :: Text
  lowers = pack ['a'..'z']
  uppers = pack ['A'..'Z']

patiently :: WD a -> WD a
patiently go = waitUntil' 0 waitTime catchEmAll
  where
  -- | I freely admit this is shameful, but this function is supposed to keep trying no matter what.
  catchEmAll = go `catch` superBadCatcher
  superBadCatcher :: SomeException -> WD a
  superBadCatcher e = unexpected $ "I failed: " <> show e

waitTime :: Double
waitTime = 60

data WDResult a = Success a | Failure
  deriving (Eq, Show)

tryWD :: WD a -> WD (WDResult a)
tryWD go = waitUntil 0 (go >>= return . Success) `onTimeout` return Failure

tryEither :: WD a -> WD b -> WD (Either a b)
tryEither getA getB = do
  a <- tryWD getA
  case a of
    Success s -> return $ Left s
    Failure   -> Right <$> getB

tryUntil :: WD a -> WD a -> WD a
tryUntil attempt until = withAsync attempt $ const untilLoop
  where
  untilLoop = go =<< tryWD until
  go result = case result of
    Success a -> return a
    Failure   -> untilLoop