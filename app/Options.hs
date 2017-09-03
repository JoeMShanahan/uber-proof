module Options
  ( Options (..)
  , withOptions
  ) where

import Options.Applicative
import System.Exit
import Uberlude

data Options = Options
  { optSeleniumServerHost :: String
  , optSeleniumServerPort :: Maybe Int
  }

data Args = Args
  { argsSeleniumServerHost :: String
  , argsSeleniumServerPort :: Maybe Int
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
argsToOptions args = Just $ Options (argsSeleniumServerHost args) (argsSeleniumServerPort args)

parseArgs :: ParserInfo Args
parseArgs =
  info (argsParser <**> helper)
    (  fullDesc
    <> progDesc "Project to document Uber trips, collect proof of a trip and compare known trips to known charges."
    )
  where
  argsParser = Args <$> serverHost <*> serverPort
  serverHost = strOption
    (  long "selenium-server-host"
    <> short 'h'
    <> metavar "HOST"
    <> help "Hostname of the Selenium server used to drive the browser"
    )
  serverPort = optional $ option auto
    (  long "selenium-server-port"
    <> short 'p'
    <> metavar "PORT"
    <> help "Port of the Selenium server used to drive the browser"
    )