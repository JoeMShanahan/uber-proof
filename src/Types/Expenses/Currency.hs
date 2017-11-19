module Types.Expenses.Currency
  ( Currency (..)
  , Pounds (..)
  , Pence

  , makePence
  , displayCurrencyValue
  ) where

import Uberlude
import Data.Text as T
data Currency = GBP Pounds Pence
  deriving (Eq, Show, Ord)

newtype Pounds = Pounds Int deriving (Eq, Show, Ord)
newtype Pence  = Pence  Int deriving (Eq, Show, Ord)

makePence :: Int -> Maybe Pence
makePence n
  | n < 0     = Nothing
  | n > 99    = Nothing
  | otherwise = Just $ Pence n

displayCurrencyValue :: Currency -> Text
displayCurrencyValue (GBP (Pounds pounds) (Pence pence)) = 
  show pounds <> "." <> pencePad
  where
  penceStr = show pence
  pencePad
    | T.length penceStr == 1 = "0" <> penceStr
    | otherwise              = penceStr