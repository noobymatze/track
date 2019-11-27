{-# LANGUAGE OverloadedStrings #-}
module CLI
  ( Command (..)
  , parser
  , run
  ) where


import qualified Config                  as Config
import           Control.Monad
import qualified Data.Custom             as Custom
import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.NewTimeEntry       (NewTimeEntry (..))
import qualified Data.NewTimeEntry       as NewTimeEntry
import qualified Data.Text               as T
import qualified Data.Time               as Time
import qualified Data.TimeEntry          as TimeEntry
import qualified Data.User               as User
import           Helper                  ((|>))
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http
import           Options.Applicative
import qualified Redmine.Client          as Client
import qualified Servant.Client          as C



-- COMMAND


data Command
  = Configure
    { _key     :: T.Text
    , _baseUrl :: T.Text
    }
  | List
  | New
    { _issueId   :: Maybe Int
    , _projectId :: Maybe Int
    , _hours     :: Double
    , _activity  :: Maybe Int
    , _comment   :: Maybe T.Text
    }



-- RUN


run :: Command -> IO ()
run cmd =
  case cmd of
    Configure key baseUrl -> do
      env <- createEnv baseUrl
      eitherUser <- C.runClientM (Client.getCurrentUser key) env
      case eitherUser of
        Left err ->
          print err

        Right user ->
          let
            config =
              Config.Config key baseUrl (User.login user) (User.id user)
          in do
            path <- Config.store config
            putStrLn $ "Configuration successfully written to " ++ path

    List ->
      listToday

    New issueId projectId hours activity comment ->
      let
        create today =
          NewTimeEntry.NewTimeEntry
          { issue_id = issueId
          , project_id = projectId
          , spent_on = today
          , hours = hours
          , activity = fromMaybe 0 activity
          , comments = comment
          , custom_fields = [] -- [Custom.CustomValue 5 "0"]
          }

      in withConfig $ \config -> do
        today <- getToday
        env <- createEnv (Config.baseUrl config)
        let newEntry = create (Just today)
        result <- C.runClientM (Client.createEntry (Config.key config) newEntry) env
        case result of
          Left err ->
            print err

          Right _ ->
            putStrLn "Successfully created your time"







listToday :: IO ()
listToday = withConfig $ \config ->
  let
    userId =
      config
        |> Config.userId

    key =
      config
        |> Config.key

    baseUrl =
      config
        |> Config.baseUrl
  in do
    env <- createEnv baseUrl
    today <- getToday
    result <- C.runClientM (Client.getEntries key today userId) env
    case result of
      Left err ->
        print err

      Right entries ->
        forM_ entries $ \entry ->
          putStrLn $ T.unpack (TimeEntry.display entry)



-- PARSER


parser :: ParserInfo Command
parser =
  info ((parserHelp <|> trackOptions) <**> helper)
    ( fullDesc
    <> progDesc "Configure and show your time from today"
    <> header "track - a small CLI to track your time with redmine"
    )


parserHelp :: Parser Command
parserHelp =
  let
    configureHelp =
      progDesc
        "Configure the base URL and API key of your Redmine instance."
  in
    subparser
    ( command "config" (info (configureOptions <**> helper) configureHelp)
    <> command "list" (info (pure List) (progDesc "List logged time of today"))
    )


trackOptions :: Parser Command
trackOptions =
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
      Just <$> option auto
        ( long "activity"
        <> short 'a'
        <> help "The activity"
        ) <|> pure Nothing

    comment =
      Just . T.pack <$> strOption
        ( long "message"
        <> short 'm'
        <> help "A message"
        ) <|> pure Nothing
  in
    New <$> issueId <*> projectId <*> hours <*> activity <*> comment


configureOptions :: Parser Command
configureOptions =
  let
    key =
      strOption
        ( long "key"
        <> short 'k'
        <> help "Your personal Redmine API Key. You can see it in your account."
        )

    baseUrl =
      strOption
        ( long "base-url"
        <> short 'b'
        <> help "The base URL of your redmine server."
        )
  in
    Configure <$> key <*> baseUrl



-- HELPERS


withConfig :: (Config.Config -> IO a) -> IO ()
withConfig f = do
  eitherConfig <- Config.load
  case eitherConfig of
    Left Config.NotFound ->
      putStrLn "Configuration could not be found, please run `track configure`!"

    Right config ->
      f config >> pure ()


createEnv :: T.Text -> IO C.ClientEnv
createEnv baseUrl = do
  baseUrl <- C.parseBaseUrl (T.unpack baseUrl)
  manager' <- Http.newManager Http.tlsManagerSettings
  pure (C.mkClientEnv manager' baseUrl)


getToday :: IO Time.Day
getToday = do
  now <- Time.getCurrentTime
  pure (Time.utctDay now)
