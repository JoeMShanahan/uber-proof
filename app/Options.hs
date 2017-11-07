module Options
  ( Options (..)
  , withOptions
  , parseDay
  ) where

import           Data.Time
import           Options.Applicative
import           Types.Uber
import           Uberlude            hiding (option)

data Options = Options
  { optSeleniumServerHost :: String
  , optSeleniumServerPort :: Maybe Int
  , optUberUser           :: Username
  , optUberPass           :: Password
  , optStart              :: Day
  , optEnd                :: Day
  }

data Args = Args
  { argsSeleniumServerHost :: String
  , argsSeleniumServerPort :: Maybe Int
  , argsUberUser           :: String
  , argsUberPass           :: String
  , argsStart              :: Maybe Day
  , argsEnd                :: Maybe Day
  }

withOptions :: (Options -> IO a) -> IO a
withOptions go = do
  args <- readArgs
  case argsToOptions args of
    Right opts -> go opts
    Left err   -> die $ describeArgsError err

readArgs :: IO Args
readArgs = execParser parseArgs

data ArgsError
  = EndBeforeStart
  | NoParseStart
  | NoParseEnd
  deriving (Eq, Show)

describeArgsError :: ArgsError -> Text
describeArgsError err = case err of
  EndBeforeStart -> "Supplied end date is before start date"
  NoParseStart   -> "Could not parse start date"
  NoParseEnd     -> "Could not parse end date"

argsToOptions :: Args -> Either ArgsError Options
argsToOptions args = do

  start <- maybe' NoParseStart $ argsStart args
  end   <- maybe' NoParseEnd   $ argsEnd   args

  when (start > end) $ Left EndBeforeStart

  return $ Options
    { optSeleniumServerHost = argsSeleniumServerHost args
    , optSeleniumServerPort = argsSeleniumServerPort args
    , optUberUser           = Username $ pack $ argsUberUser args
    , optUberPass           = Password $ pack $ argsUberPass args
    , optStart              = start
    , optEnd                = end
    }
  where
  maybe' l = maybe (Left l) Right

parseArgs :: ParserInfo Args
parseArgs =
  info (argsParser <**> helper)
    (  fullDesc
    <> progDesc "Project to document Uber trips, collect proof of a trip and compare known trips to known charges."
    )
  where
  argsParser =

        Args
    <$> serverHost
    <*> serverPort
    <*> username
    <*> password
    <*> start
    <*> end

  serverHost = strOption
    (  long "selenium-server-host"
    <> short 'h'
    <> metavar "HOST"
    <> help "Hostname of the Selenium server used to drive the browser"
    )
  serverPort = optional $ option auto
    (  long "selenium-server-port"
    <> metavar "PORT"
    <> help "Port of the Selenium server used to drive the browser"
    )
  username = strOption
    (  long "uber-username"
    <> short 'u'
    <> metavar "USERNAME"
    <> help "Username to log in to Uber with"
    )
  password = strOption
    (  long "uber-password"
    <> short 'p'
    <> metavar "PASSWORD"
    <> help "Password to log in to Uber with"
    )
  start = parseDay <$> strOption
    (  long "start"
    <> metavar "DAY"
    <> help "Get trips from this day"
    )
  end = parseDay <$> strOption
    (  long "end"
    <> metavar "DAY"
    <> help "Get trips up to day this day"
    )

parseDay :: String -> Maybe Day
parseDay s = asum $ map tryFormat ["%D", "%F", "%x"]
  where
  tryFormat formatString = parseTimeM True defaultTimeLocale formatString s
