module CLI.Authenticated
  ( Command
  , parser
  , run
  ) where


import qualified App
import qualified Config
import           Control.Monad        (forM_)
import           Control.Monad.Reader (liftIO)
import qualified Data.Custom          as Custom
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Data.NewTimeEntry    (NewTimeEntry (..))
import qualified Data.NewTimeEntry    as NewTimeEntry
import qualified Data.Text            as T
import qualified Data.Time            as Time
import qualified Data.TimeEntry       as TimeEntry
import           Options.Applicative
import qualified Redmine.Client       as Client



-- COMMAND


data Command
  = New
  | List



-- RUN


run :: Config.Config -> Command -> App.App ()
run config cmd =
  case cmd of
    New ->
      undefined

    List ->
      let
        userId =
          Config.userId config
      in do
        today   <- getToday
        entries <- Client.getEntries today userId
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
  parserHelp <|> new


parserHelp :: Parser Command
parserHelp =
  subparser
    ( command "list" (info (pure List) (progDesc "List logged time of today"))
    )


new :: Parser Command
new =
  let
    --issueId =
    --  Just <$> option auto
    --    ( long "issue"
    --    <> short 'i'
    --    <> help "The issue id you have been working on"
    --    ) <|> pure Nothing

    --projectId =
    --  Just <$> option auto
    --    ( long "project"
    --    <> short 'p'
    --    <> help "The project id you have been working on"
    --    ) <|> pure Nothing

    --hours =
    --  option auto
    --    ( long "hours"
    --    <> short 't'
    --    <> help "The number of hours"
    --    )

    --activity =
    --  Just <$> option auto
    --    ( long "activity"
    --    <> short 'a'
    --    <> help "The activity"
    --    ) <|> pure Nothing

    --comment =
    --  Just . T.pack <$> strOption
    --    ( long "message"
    --    <> short 'm'
    --    <> help "A message"
    --    ) <|> pure Nothing
  in
    pure New -- <$> issueId <*> projectId <*> hours <*> activity <*> comment

    --New issueId projectId hours activity comment ->
      --let
        --create today =
          --NewTimeEntry.NewTimeEntry
          --{ issue_id = issueId
          --, project_id = projectId
          --, spent_on = today
          --, hours = hours
          --, activity = fromMaybe 0 activity
          --, comments = comment
          --, custom_fields = [] -- [Custom.CustomValue 5 "0"]
          --}
--
      --in withConfig $ \config -> do
        --today <- getToday
        --env <- createEnv (Config.baseUrl config)
        --let newEntry = create (Just today)
        --result <- C.runClientM (Client.createEntry (Config.key config) newEntry) env
        --case result of
          --Left err ->
            --print err
--
          --Right _ ->
            --putStrLn "Successfully created your time"


--parserHelp :: Maybe Config.Config -> Parser Command
--parserHelp maybeConfig =
--  let
--    configureHelp =
--      progDesc
--        "Configure the base URL and API key of your Redmine instance."
--  in
--    subparser
--    ( command "init" (info (configureOptions <**> helper) configureHelp)
--    <> command "list" (info (pure List) (progDesc "List logged time of today"))
--    )

