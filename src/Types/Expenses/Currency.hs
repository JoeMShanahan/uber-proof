module Types.Expenses.Currency
  ( Currency (..)
  , Pounds (..)
  , Pence

  , makePence
  , displayCurrencyValue
  ) where

import           Data.Text as T
import           Uberlude

data Currency = GBP Pounds Pence
  deriving (Eq, Show, Ord)

newtype Pounds = Pounds Int deriving (Eq, Show, Ord)
newtype Pence  = Pence  Int deriving (Eq, Show, Ord)

instance Monoid Currency where
  mappend = addCurrencies
  mempty  = GBP (Pounds 0) (Pence 0)

addCurrencies :: Currency -> Currency -> Currency
addCurrencies (GBP (Pounds po1) (Pence pe1)) (GBP (Pounds po2) (Pence pe2)) =
  case makePence leftOverPence of
    Just pence -> GBP (Pounds $ po1 + po2 + newPoundsFromPence) pence
    Nothing    -> mempty
  where
  totalPence = pe1 + pe2
  newPoundsFromPence = totalPence `div` 100
  leftOverPence      = totalPence `mod` 100

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
