{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}
module App
  ( App
  , Env
  , formatError
  , client
  , createEnv
  , getApiKey
  , run
  ) where


import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson              as Json
import qualified Data.Maybe              as Maybe
import           Data.Proxy              (Proxy (..))
import qualified Data.Text               as T
import           Helper                  ((|>))
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Redmine.Errors          as Errors
import qualified Servant.Client          as Servant



-- APP


newtype App a
  = App (ReaderT Env (ExceptT Error IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError Error, MonadIO)


data Env
  = Env
    { _clientEnv :: Servant.ClientEnv
    , _key       :: T.Text
    }


data Error
  = ClientError Servant.ClientError
  deriving (Show)



-- PUBLIC HELPERS


formatError :: Error -> T.Text
formatError err =
  case err of
    ClientError (Servant.ConnectionError ex) ->
      T.intercalate ""
        [ "Couldn't connect to the server. Are you sure, you "
        , "are connected to the internet? Here is the full exception: \n"
        , "\n"
        , T.pack (show ex)
        ]

    ClientError (Servant.DecodeFailure _ response) ->
      let
        responseText =
          response
            |> Servant.responseBody
            |> Json.decode -- This is a hack.
      in
        T.intercalate ""
          [ "We could not decode the response from the server. Here it is\n"
          , "\n"
          , Maybe.fromMaybe "" responseText
          , "\n"
          , "This is usually a programming error, so go ahead and create an "
          , "issue for it."
          ]

    ClientError (Servant.FailureResponse _ response) ->
      case Json.decode (Servant.responseBody response) of
        Nothing ->
          T.intercalate ""
            [ "Your request failed for unknown reasons. Sorry, here is the full error."
            , T.pack (show err)
            ]

        Just errors ->
          "Your request failed for the following reasons:\n" <> Errors.display errors

    _ ->
      T.pack (show err)


getApiKey :: App T.Text
getApiKey =
  asks _key


createEnv :: T.Text -> T.Text -> IO Env
createEnv key baseUrl = do
  baseUrl' <- Servant.parseBaseUrl (T.unpack baseUrl)
  manager' <- Http.newManager Http.tlsManagerSettings
  let clientEnv = Servant.mkClientEnv manager' baseUrl'
  pure (Env clientEnv key)


client :: Servant.HasClient Servant.ClientM api => Proxy api -> Servant.Client App api
client api =
  let
    toApp :: Servant.ClientM a -> App a
    toApp clientM = do
      env <- asks _clientEnv
      result <- liftIO $ Servant.runClientM clientM env
      case result of
        Left err ->
          throwError (ClientError err)

        Right value ->
          pure value
  in
    Servant.hoistClient api toApp (Servant.client api)



-- RUNNING


run :: Env -> App a -> IO (Either Error a)
run env (App app) =
  runExceptT (runReaderT app env)
