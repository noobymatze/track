{-# LANGUAGE OverloadedStrings #-}
module CLI
  ( Command (..)
  , parser
  , run
  ) where


import qualified App
import qualified CLI.Authenticated   as Authenticated
import qualified Config
import qualified Data.Text           as T
import qualified Data.User           as User
import           Helper              ((|>))
import           Options.Applicative
import qualified Redmine.Client      as Client



-- COMMAND


data Command
  = Init (Maybe InitOptions)
  | Authenticated Config.Config Authenticated.Command


data InitOptions = InitOptions
  { _key     :: T.Text
  , _baseUrl :: T.Text
  }



-- RUN


run :: Command -> IO ()
run cmd =
  case cmd of
    Init (Just (InitOptions key baseUrl)) ->
      let
        newConfig user =
          Config.Config key baseUrl (User.login user) (User.id user)
      in do
        user <- execApp key baseUrl Client.getCurrentUser
        path <- Config.store (newConfig user)
        putStrLn $ "Configuration successfully written to " ++ path

    Authenticated config subCmd ->
      let
        key =
          Config.key config

        baseUrl =
          Config.baseUrl config
      in
        subCmd
          |> Authenticated.run config
          |> execApp key baseUrl

    _ ->
      putStrLn $ T.unpack $ T.intercalate "\n"
        [ "Hi, thank you for using track!"
        , ""
        , "It seems you have yet to initialize track with some credentials to use it."
        , "Please run "
        , ""
        , "    track init "
        , ""
        , "to create a ~/.track configuration."
        ]



-- PARSER


parser :: Maybe Config.Config -> ParserInfo Command
parser maybeConfig =
  let
    parserHelp =
      case maybeConfig of
        Nothing ->
          initParser <|> pure (Init Nothing)

        Just config ->
          Authenticated config <$> Authenticated.parser <|> initParser
  in
    info (parserHelp <**> helper)
      ( fullDesc
      <> progDesc "Configure and show your time from today"
      <> header "track - a small CLI to track your time with redmine"
      )


initParser :: Parser Command
initParser =
  let
    initp =
      Init <$> (Just <$> initOptions <**> helper)
  in
    subparser
    ( command "init" (info initp (progDesc "Initialize track with some configuration" ))
    )


initOptions :: Parser InitOptions
initOptions =
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
    InitOptions <$> key <*> baseUrl



-- HELPERS


execApp :: T.Text -> T.Text -> App.App a -> IO a
execApp key baseUrl app = do
  env <- App.createEnv key baseUrl
  result <- App.run env app
  case result of
    Left err ->
      error (show err)

    Right v ->
      pure v
