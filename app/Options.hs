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

withOptions :: IO a -> IO a
withOptions = identity
