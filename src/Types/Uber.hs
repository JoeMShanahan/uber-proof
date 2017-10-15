module Types.Uber
  ( UberTrip (..)

  , Username (..)
  , Password (..)

  , Month (..)
  , Year (..)

  , TripId
  , tripIdFromText
  ) where

import Data.Time
import Data.UUID
import Types.Expenses
import Uberlude

newtype Username = Username Text
newtype Password = Password Text

newtype Month = Month Int     deriving (Eq, Show)
newtype Year  = Year  Integer deriving (Eq, Show)

data UberTrip = UberTrip
  { uberTripId     :: TripId
  , uberScreenshot :: ByteString -- ^ PNG encoded
  , uberStartTime  :: UTCTime
  , uberStartLoc   :: Text
  , uberEndLoc     :: Text
  , uberCost       :: Int
  , userCard       :: BankCard
  } deriving (Eq, Show)

newtype TripId = TripId UUID
  deriving (Eq, Show)

tripIdFromText :: Text -> Maybe TripId
tripIdFromText = fmap TripId . fromString . unpack
