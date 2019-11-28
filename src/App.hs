{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}
module App
  ( App
  , Env
  , client
  , createEnv
  , getApiKey
  , run
  ) where


import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Proxy              (Proxy (..))
import qualified Data.Text               as T
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http
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
