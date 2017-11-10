import           Data.Time
import           Options
import           Test.Hspec
import           Types.Uber
import           Types.Expenses
import           Uberlude
import           UberScrape
import Data.Attoparsec.Text

main :: IO ()
main = hspec $ do

  describe "Year and months" $ do
    it "Single day returns only one pair" $ do
      let d = fromGregorian 2017 01 01
      yearAndMonths d d `shouldMatchList` [(Year 2017, Month 01)]

    it "Period within same month returns one pair" $ do
      let d1 = fromGregorian 2017 01 01
          d2 = fromGregorian 2017 01 31
      yearAndMonths d1 d2 `shouldMatchList` [(Year 2017, Month 01)]

    it "Period spanning two years captures both years" $ do
      let d1 = fromGregorian 2016 12 31
          d2 = fromGregorian 2017 01 01
      yearAndMonths d1 d2 `shouldMatchList` [(Year 2016, Month 12), (Year 2017, Month 01)]

    it "Whole year captures all months" $ do
      let d1 = fromGregorian 2017 01 01
          d2 = fromGregorian 2017 12 31
      yearAndMonths d1 d2 `shouldMatchList` map (\m -> (Year 2017, Month m)) [01 .. 12]

    it "Backwards range returns nothing" $ do
      let d = fromGregorian 2017 01 01
      yearAndMonths d (pred d) `shouldMatchList` []

  describe "Parse a day" $ do
    let expectedDay = fromGregorian 2017 01 01
        cases = [ "01/01/17"
                , "2017-01-01"
                ]

    forM_ cases $ \d -> it ("Parses " <> d) $ parseDay d `shouldBe` Just expectedDay

  describe "Parse a time from uber" $ do
    let examples = [ "4:29 PM on August 24, 2017"
                   , "5:11 PM on November 7, 2017"
                   , "8:06 AM on November 7, 2017"
                   ]
    forM_ examples $ \s -> it ("Parses \"" <> s <> "\"") $ unless (isRight $ parseUberTime $ pack s) $
      expectationFailure $ s <> " - did not parse"

  describe "Parse some monies" $ mapM_ runCurrencyTest
    [ CurrencyParses "£3.33"
    , CurrencyParses "£3"
    , CurrencyParses "£0"

    , CurrencyParseFails "abc"
    , CurrencyParseFails "$20"
    , CurrencyParseFails "£1.102"

    , CurrencyDisplayAs "£3.33" "£3.33"
    , CurrencyDisplayAs "£3"    "£3.00"
    , CurrencyDisplayAs "£0"    "£0.00"
    ]

data CurrencyTest
  = CurrencyParses Text
  | CurrencyParseFails Text
  | CurrencyDisplayAs Text Text

runCurrencyTest :: CurrencyTest -> SpecWith ()
runCurrencyTest test = case test of
  CurrencyParses input ->
    it (unpack $ "Parse of \"" <> input <> "\" succeeds") $ case doParse input of
      Right _  -> return ()
      Left err -> expectationFailure $ "Parse failed: " <> err

  CurrencyParseFails input ->
    it (unpack $ "Parse of \"" <> input <> "\" fails") $ case doParse input of
      Right _ -> expectationFailure "Parse succeeded"
      Left _  -> return ()

  CurrencyDisplayAs input output ->
    it (unpack $ "Parse of \"" <> input <> "\" displays as \"" <> output <> "\"") $ case doParse input of 
      Right curr -> displayCurrency curr `shouldBe` output
      Left err   -> expectationFailure $ "Parse failed: " <> err
  where
  doParse = parseOnly parseGBP