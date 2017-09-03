module Options
  ( Options
  , withOptions
  ) where

import Options.Applicative
import Network.URI
import System.Exit
import Uberlude

newtype Options = Options
  { optSeleniumServerHost :: URI
  }

newtype Args = Args
  { argsSeleniumServerHost :: String
  }

withOptions :: (Options -> IO a) -> IO a
withOptions go = do
  args <- readArgs
  case argsToOptions args of
    Just opts -> go opts
    Nothing   -> die "Could not make options from args"

readArgs :: IO Args
readArgs = execParser parseArgs

argsToOptions :: Args -> Maybe Options
argsToOptions args = Options <$> parseURI (argsSeleniumServerHost args)

parseArgs :: ParserInfo Args
parseArgs =
  info (argsParser <**> helper)
    (  fullDesc
    <> progDesc "Project to document Uber trips, collect proof of a trip and compare known trips to known charges."
    )
  where
  argsParser = Args <$> serverRaw
  serverRaw = strOption
    (  long "selenium-server-uri"
    <> short 'u'
    <> metavar "URI"
    <> help "URI to the Selenium server used to drive the browser"
    )