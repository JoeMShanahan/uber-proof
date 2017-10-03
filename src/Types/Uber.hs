module Types.Uber
  ( UberTrip (..)

  , Username (..)
  , Password (..)

  , Month (..)
  , Year (..)
  ) where

import Data.Time
import Types.Expenses
import Uberlude

newtype Username = Username Text
newtype Password = Password Text

newtype Month = Month Int     deriving (Eq, Show)
newtype Year  = Year  Integer deriving (Eq, Show)

data UberTrip = UberTrip
  { uberTripId     :: Text       -- ^ This is a v4 UUID
  , uberScreenshot :: ByteString -- ^ PNG encoded
  , uberStartTime  :: UTCTime
  , uberStartLoc   :: Text
  , uberEndLoc     :: Text
  , uberCost       :: Int
  , userCard       :: BankCard
  } deriving (Eq, Show)
