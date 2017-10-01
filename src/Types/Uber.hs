module Types.Uber
  ( UberTrip (..)

  , Username (..)
  , Password (..)
  ) where

import Data.Time
import Types.Expenses
import Uberlude

newtype Username = Username Text
newtype Password = Password Text

data UberTrip = UberTrip
  { uberScreenshot :: ByteString
  , uberStartTime  :: UTCTime
  , uberStartLoc   :: Text
  , uberEndLoc     :: Text
  , uberCost       :: Int
  , userCard       :: BankCard
  } deriving (Eq, Show)
