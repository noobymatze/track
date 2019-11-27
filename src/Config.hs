{-# LANGUAGE DeriveGeneric #-}
module Config
  ( Config (..)
  , Error (..)
  , load
  , store
  ) where


import           Data.Aeson       as Json
import qualified Data.Text        as T
import           GHC.Generics
import           Helper           ((|>))
import qualified System.Directory as D
import           System.FilePath



-- CONFIG


data Config
  = Config
    { key     :: T.Text
    , baseUrl :: T.Text
    , login   :: T.Text
    , userId  :: Int
    } deriving (Generic)


data Error
  = NotFound



-- WORK WITH THE CONFIGURATION


load :: IO (Either Error Config)
load =
  let
    toEither maybeConfig =
      case maybeConfig of
        Nothing ->
          Left NotFound

        Just config ->
          Right config
  in do
    configFile <- getConfigFile
    exists     <- D.doesFileExist configFile
    if not exists then
      pure (Left NotFound)
    else
      configFile
        |> Json.decodeFileStrict
        |> fmap toEither


store :: Config -> IO FilePath
store config = do
  configFile <- getConfigFile
  Json.encodeFile configFile config
  pure configFile



-- HELPERS


getConfigFile :: IO FilePath
getConfigFile = do
  home <- D.getHomeDirectory
  pure (home </> ".track")



-- SERIALIZATION


instance FromJSON Config where
instance ToJSON Config where
