module Main where


import           CLI
import qualified Config
import qualified Options.Applicative as Options
import           System.IO



-- MAIN


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  Config.load >>= Options.execParser . CLI.parser >>= CLI.run
