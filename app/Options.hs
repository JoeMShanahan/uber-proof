module Options
  ( Options
  , withOptions
  ) where

import Protolude
import Options.Applicative

data Options = Options
  { optSeleniumServer :: FilePath
  , optWebdriver      :: WebDriver
  }

data WebDriver = Chrome FilePath

withOptions :: (Options -> IO a) -> IO a
withOptions go = getOptions >>= go

getOptions :: IO Options
getOptions = execParser parseArgs

parseArgs :: ParserInfo Options
parseArgs =
  info (optionsParser <**> helper)
    (  fullDesc
    <> progDesc "Project to document Uber trips, collect proof of a trip and compare known trips to known charges."
    <> header "My header"
    )
  where
  optionsParser = Options <$> getSelenium <*> getWebDriver
  getKey = strOption
    (  long "key"
    <> short 'k'
    <> metavar "KEY"
    <> help "Your API key"
    )
  getFilePath = strOption
    (  long "key-file"
    <> short 'f'
    <> metavar "KEY_FILE"
    <> help "Location of your key file (utf8 encoding)"
    )
  getVerbosity =
    flag undefined undefined
    (  long "verbose"
    <> short 'v'
    <> help "Enably verbose mode"
    )
  getSelenium = undefined
  getWebDriver = undefined