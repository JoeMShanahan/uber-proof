module Types.Expenses.Currency
  ( Currency (..)
  , Pounds
  , Pence

  , makePence
  , displayCurrency
  ) where

import Uberlude

data Currency = GBP Pounds Pence
  deriving (Eq, Show, Ord)

newtype Pounds = Pounds Int deriving (Eq, Show, Ord)
newtype Pence  = Pence  Int deriving (Eq, Show, Ord)

makePence :: Int -> Maybe Pence
makePence n
  | n < 0     = Nothing
  | n > 99    = Nothing
  | otherwise = Just $ Pence n

displayCurrency :: Currency -> Text
displayCurrency (GBP (Pounds pounds) (Pence pence)) = 
  "£" <> show pounds <> "." <> show pence