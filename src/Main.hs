module Main where

import           CLI
import qualified Config
import qualified Options.Applicative as Options



-- MAIN


main :: IO ()
main =
  Config.load >>= Options.execParser . CLI.parser >>= CLI.run
