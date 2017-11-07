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
  ) where

import           Data.Hashable
import           Data.Time
import           Data.UUID
import           Types.Expenses
import           Uberlude

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
  deriving (Eq, Show, Hashable)

tripIdFromText :: Text -> Maybe TripId
tripIdFromText = fmap TripId . fromString . unpack

tripIdToString :: TripId -> String
tripIdToString (TripId uuid) = toString uuid

tripIdText :: TripId -> Text
tripIdText = pack . tripIdToString