{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Uber
  ( UberTrip (..)

  , Username (..)
  , Password (..)

  , Month (..)
  , Year (..)

  , TripId
  , tripIdFromText
  , tripIdToString
  , tripIdText

  , parseUberTime
  , parseUberTimeOfDay
  ) where

import           Data.Hashable
import           Data.Time
import           Data.UUID
import           Types.Expenses
import           Uberlude

newtype Username = Username Text
newtype Password = Password Text

newtype Month = Month { unMonth :: Int     } deriving (Eq, Show)
newtype Year  = Year  { unYear  :: Integer } deriving (Eq, Show)

data UberTrip = UberTrip
  { uberTripId     :: TripId
  , uberScreenshot :: ByteString -- ^ PNG encoded
  , uberStartTime  :: UTCTime
  , uberEndTime    :: UTCTime
  , uberStartLoc   :: Text
  , uberEndLoc     :: Text
  , uberCost       :: Currency
  , uberCard       :: BankCard
  , uberProfile    :: Text
  } deriving (Eq, Show)

newtype TripId = TripId UUID
  deriving (Eq, Show, Hashable)

tripIdFromText :: Text -> Maybe TripId
tripIdFromText = fmap TripId . fromString . unpack

tripIdToString :: TripId -> String
tripIdToString (TripId uuid) = toString uuid

tripIdText :: TripId -> Text
tripIdText (TripId uuid) = toText uuid

parseUberTime :: Text -> Either String LocalTime
parseUberTime = asum . zipWith tryParse formats . repeat . unpack
  where
  formats = [todFormat <> " on %B %e, %Y"]
  tryParse = parseTimeM True defaultTimeLocale

parseUberTimeOfDay :: Text -> Either String TimeOfDay
parseUberTimeOfDay = parseTimeM True defaultTimeLocale todFormat . unpack
  where

todFormat :: String
todFormat = "%l:%M %p"
