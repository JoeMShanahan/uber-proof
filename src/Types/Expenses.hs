module Types.Expenses
  ( BankCard
  , CardType (..)
  , makeCard
  , parseGBP
  ) where

import           Uberlude

data BankCard = Card CardType Int
  deriving (Eq, Show)

data CardType = Visa | MasterCard
  deriving (Eq, Show)

makeCard :: CardType -> Int -> Maybe BankCard
makeCard cardType last4Digits
  | last4Digits < 0    = Nothing
  | last4Digits > 9999 = Nothing
  | otherwise          = Just $ Card cardType last4Digits

parseGBP :: Text -> Maybe Int
parseGBP = undefined
