{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Expenses
  ( module Reexported

  , BankCard
  , cardText
  , CardType (..)
  , makeCard
  , parseGBP
  ) where

import           Data.Attoparsec.Text    as AP
import           Types.Expenses.Currency as Reexported
import           Uberlude

data BankCard = Card CardType Int
  deriving (Eq, Show)

data CardType = Visa | MasterCard
  deriving (Eq, Show)

cardText :: BankCard -> Text
cardText (Card cardType num) =
  typeToText cardType <> "-" <> show num
  where
  typeToText = \case
    Visa       -> "visa"
    MasterCard -> "mc"

makeCard :: CardType -> Int -> Maybe BankCard
makeCard cardType last4Digits
  | last4Digits < 0    = Nothing
  | last4Digits > 9999 = Nothing
  | otherwise          = Just $ Card cardType last4Digits

parseGBP :: Parser Currency
parseGBP = do
  void $ char '£'
  poundsNum <- decimal
  penceNum <- endInputPence <|> getNonZeroPence
  GBP (Pounds poundsNum) <$> parsePence penceNum
  where
  parsePence    = maybe mzero return . makePence
  endInputPence = do
    mChar <- peekChar
    case mChar of
     Nothing -> return 0
     Just c  -> fail $ "Expected end, found " <> [c]
  getNonZeroPence = char '.' >> decimal

