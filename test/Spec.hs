import Protolude
import Test.Hspec
import Types.Uber
import UberScrape
import Data.Time

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