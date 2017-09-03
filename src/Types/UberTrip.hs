module Types.UberTrip
  ( UberTrip (..)
  ) where

import Data.Time
import Types.Expenses
import Uberlude

data UberTrip = UberTrip
  { uberScreenshot :: ByteString
  , uberStartTime  :: UTCTime
  , uberStartLoc   :: Text
  , uberEndLoc     :: Text
  , uberCost       :: Int
  , userCard       :: BankCard
  } deriving (Eq, Show)
