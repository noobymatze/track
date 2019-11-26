{-# LANGUAGE DeriveGeneric #-}
module Config
  ( Config (..)
  , Error (..)
  , load
  , write
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



-- READ CONFIGURATION


load :: IO (Either Error Config)
load = withConfigFile $ \configFile ->
  let
    toEither maybeConfig =
      case maybeConfig of
        Nothing ->
          Left NotFound

        Just config ->
          Right config
  in do
    exists <- D.doesFileExist configFile
    if not exists then
      pure (Left NotFound)
    else
      configFile
        |> Json.decodeFileStrict
        |> fmap toEither


write :: Config -> IO FilePath
write config =
  withConfigFile $ \configFile ->
    Json.encodeFile configFile config >> pure configFile



-- HELPERS


withConfigFile :: (FilePath -> IO a) -> IO a
withConfigFile f = do
  home <- D.getHomeDirectory
  f (home </> ".track")



-- SERIALIZATION


instance FromJSON Config where
instance ToJSON Config where
