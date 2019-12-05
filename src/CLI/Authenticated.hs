{-# LANGUAGE OverloadedStrings #-}
module CLI.Authenticated
  ( Command
  , parser
  , run
  ) where


import qualified App
import qualified Config
import           Control.Monad                    (forM_, void)
import           Control.Monad.Reader             (liftIO)
import qualified Data.Text                        as T
import qualified Data.Time                        as Time
import           Helper                           ((|>))
import           Options.Applicative
import qualified Prompt
import qualified Redmine.CustomFields.Client      as CustomFields
import qualified Redmine.CustomFields.CustomField as CustomField
import qualified Redmine.TimeEntries.Client       as TimeEntries
import           Redmine.TimeEntries.NewTimeEntry as NewTimeEntry
import qualified Redmine.TimeEntries.TimeEntry    as TimeEntry
import qualified System.Console.ANSI              as ANSI



-- COMMAND


data Command
  = New Bool
  | List



-- RUN


run :: Config.Config -> Command -> App.App ()
run config cmd =
  case cmd of
    New allCustomFields ->
      let
        shouldPromptFor field =
          CustomField.isForTimeEntry field
            && (CustomField.isRequired field || allCustomFields)
      in do
        today <- getToday
        activities <- TimeEntries.getActivities
        customFields <- filter shouldPromptFor <$> CustomFields.getAll
        result <- liftIO $ Prompt.run $ NewTimeEntry.prompt today activities customFields
        TimeEntries.create result

    List -> do
      today   <- getToday
      entries <- pure [] -- TimeEntries.find today (Config.userId config)
      let totalHours  = TimeEntry.totalHours entries
      let options     = TimeEntry.createOptions entries
      let viewed      = fmap (TimeEntry.display options) entries
      let longestLine = 8 + maxi (fmap (T.length . T.intercalate " " . fmap snd) viewed)
      if null entries then do
        liftIO $ putStrLn "----------------"
        liftIO $ putStrLn "| No entries yet"
        liftIO $ putStrLn "----------------"
      else do
        liftIO $ putStrLn $ sep longestLine
        forM_ entries (liftIO . viewEntry options)
        liftIO $ putStrLn $ sep longestLine
      liftIO $ setColorForFullHours totalHours
      liftIO $ putStrLn $ "| Total:  " ++ show totalHours


getToday :: App.App Time.Day
getToday = do
  now <- liftIO Time.getCurrentTime
  pure (Time.utctDay now)


viewEntry :: TimeEntry.DisplayOptions -> TimeEntry.TimeEntry -> IO ()
viewEntry opts entry =
  let
    view i (color, column) =
      let
        indent =
          if i == 0 then "" else " "
      in do
        ANSI.setSGR [ANSI.Reset]
        putStr $ T.unpack $ indent <> "| "
        ANSI.setSGR [color]
        putStr $ T.unpack column
  in
  entry
    |> TimeEntry.display opts
    |> zipWith view [0..]
    |> sequence
    >> putStrLn ""


maxi :: (Ord a, Num a) => [a] -> a
maxi [] = 0
maxi a  = maximum a


setColorForFullHours :: Double -> IO ()
setColorForFullHours hours
  | 8.0 <= hours && hours <= 9.0 =
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  | hours > 9.0 =
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
  | otherwise =
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]


sep :: Int -> String
sep i =
  replicate i '-'



-- PARSER


parser :: Parser Command
parser =
  let
    subCmd =
      subparser
        ( command "list" (info (pure List) (progDesc "List logged time of today"))
        )
  in
    subCmd <|> new


new :: Parser Command
new =
  let
    allCustomFields =
      switch
        ( long "all-custom-fields"
        <> short 'a'
        <> help "Prompts for all defined custom fields not only the required"
        )
  in
    New <$> allCustomFields
