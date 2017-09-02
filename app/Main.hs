module Main where

import Protolude
import Options

main :: IO ()
main = withOptions $ \opt -> do
  putText "Done!"