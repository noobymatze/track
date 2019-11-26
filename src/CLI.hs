module CLI
  ( Command (..)
  , parser
  , run
  ) where


import qualified Config                  as Config
import           Control.Monad
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
    , _login   :: T.Text
    }
  | List



-- RUN


run :: Command -> IO ()
run cmd =
  case cmd of
    Configure key baseUrl login -> do
      env <- createEnv baseUrl
      eitherUser <- C.runClientM (Client.getUser key login) env
      case eitherUser of
        Left err ->
          print err

        Right Nothing ->
          putStrLn "User could not be found, did you really user your login name?"

        Right (Just user) ->
          let
            config =
              Config.Config key baseUrl login (User.id user)
          in do
            path <- Config.write config
            putStrLn $ "Configuration successfully written to " ++ path ++ "!"

    List ->
      listToday


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
  info (parserHelp <**> helper)
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
    ( command "configure" (info (configureOptions <**> helper) configureHelp)
    <> command "list" (info (pure List) (progDesc "List logged time of today"))
    )


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

    login =
      strOption
        ( long "login"
        <> short 'l'
        <> help "The login name for your personal Redmine account."
        )
  in
    Configure <$> key <*> baseUrl <*> login



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
