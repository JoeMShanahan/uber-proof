module Main where

import           Options
import           Uberlude
import           UberScrape
import           Types.Expenses.Currency
import           Types.Uber
import qualified Data.ByteString as BS

main :: IO ()
main = withOptions $ \opt -> do
  let host      = optSeleniumServerHost opt
      port      = optSeleniumServerPort opt
      user      = optUberUser opt
      password  = optUberPass opt
      start     = optStart opt
      end       = optEnd opt

  result <- getTrips start end host port user password

  reportFailures $ concatMap tripsFailed result
  processTrips $ concatMap tripsRetrieved result

reportFailures :: [TripRetrievalFailure] -> IO ()
reportFailures = mapM_ $ putText . describeFailure
    
processTrips :: [UberTrip] -> IO ()
processTrips trips = do
  forM_ trips $ \trip -> BS.writeFile (show (uberTripId trip) <> ".png") (uberScreenshot trip)
  putText $ displayCurrencyValue $ mconcat $ map uberCost trips