{-# LANGUAGE OverloadedStrings #-}
module CLI.Authenticated
  ( Command
  , parser
  , run
  ) where


import qualified App
import qualified Config
import           Control.Monad                    (forM_)
import           Control.Monad.Reader             (liftIO)
import qualified Data.Text                        as T
import qualified Data.Time                        as Time
import           Options.Applicative
import qualified Prompt
import qualified Redmine.CustomFields.Client      as CustomFields
import qualified Redmine.CustomFields.CustomField as CustomField
import qualified Redmine.TimeEntries.Client       as TimeEntries
import           Redmine.TimeEntries.NewTimeEntry as NewTimeEntry
import qualified Redmine.TimeEntries.TimeEntry    as TimeEntry



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
      entries <- TimeEntries.find today (Config.userId config)
      let allHours = sum (fmap TimeEntry.hours entries)
      liftIO $ putStrLn $ "Hours: " ++ show allHours
      forM_ entries $ \entry ->
        liftIO $ putStrLn $ T.unpack (TimeEntry.display entry)


getToday :: App.App Time.Day
getToday = do
  now <- liftIO Time.getCurrentTime
  pure (Time.utctDay now)



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
