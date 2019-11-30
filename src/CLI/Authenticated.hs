module CLI.Authenticated
  ( Command
  , parser
  , run
  ) where


import qualified App
import qualified Config
import           Control.Monad                    (forM_)
import           Control.Monad.Reader             (liftIO)
import qualified Data.Maybe                       as Maybe
import qualified Data.Text                        as T
import qualified Data.Time                        as Time
import           Options.Applicative
import qualified Redmine.CustomFields.Client      as CustomFields
import qualified Redmine.CustomFields.CustomField as CustomField
import qualified Redmine.TimeEntries.Client       as TimeEntries
import           Redmine.TimeEntries.NewTimeEntry (NewTimeEntry (..))
import qualified Redmine.TimeEntries.TimeEntry    as TimeEntry



-- COMMAND


data Command
  = New
    { _issueId   :: Maybe Int
    , _projectId :: Maybe Int
    , _hours     :: Double
    , _activity  :: Int
    , _comment   :: Maybe T.Text
    }
  | List



-- RUN


run :: Config.Config -> Command -> App.App ()
run config cmd =
  case cmd of
    New issueId projectId hours activity comment -> do
      today <- getToday
      customFields <- filter CustomField.isForTimeEntry <$> CustomFields.getAll
      customValues <- traverse (liftIO . CustomField.prompt) customFields
      TimeEntries.create $
        NewTimeEntry
          { newIssueId = issueId
          , newProjectId = projectId
          , newSpentOn = Just today
          , newHours = hours
          , newActivity = activity
          , newComments = comment
          , newCustomValues = Maybe.catMaybes customValues
          }

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
    issueId =
      Just <$> option auto
        ( long "issue"
        <> short 'i'
        <> help "The issue id you have been working on"
        ) <|> pure Nothing

    projectId =
      Just <$> option auto
        ( long "project"
        <> short 'p'
        <> help "The project id you have been working on"
        ) <|> pure Nothing

    hours =
      option auto
        ( long "hours"
        <> short 't'
        <> help "The number of hours"
        )

    activity =
      option auto
        ( long "activity"
        <> short 'a'
        <> help "The activity"
        )

    comment =
      Just . T.pack <$> strOption
        ( long "message"
        <> short 'm'
        <> help "A message"
        ) <|> pure Nothing
  in
    New <$> issueId <*> projectId <*> hours <*> activity <*> comment
