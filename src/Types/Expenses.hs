module Types.Expenses
  ( module Reexported

  , BankCard
  , CardType (..)
  , makeCard
  , parseGBP
  ) where

import           Uberlude
import Data.Attoparsec.Text as AP
import Types.Expenses.Currency as Reexported

data BankCard = Card CardType Int
  deriving (Eq, Show)

data CardType = Visa | MasterCard
  deriving (Eq, Show)

makeCard :: CardType -> Int -> Maybe BankCard
makeCard cardType last4Digits
  | last4Digits < 0    = Nothing
  | last4Digits > 9999 = Nothing
  | otherwise          = Just $ Card cardType last4Digits

parseGBP :: Parser Currency
parseGBP = do
  void $ char '£'
  poundsNum <- decimal
  penceNum <- AP.option 0 $ char '.' >> decimal
  GBP (Pounds poundsNum) <$> parsePence penceNum
  where
  parsePence = maybe mzero return . makePence

