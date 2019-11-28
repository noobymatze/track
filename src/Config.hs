{-# LANGUAGE DeriveGeneric #-}
module Config
  ( Config (..)
  , load
  , store
  ) where


import           Data.Aeson       as Json
import qualified Data.Text        as T
import           GHC.Generics
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



-- WORK WITH THE CONFIGURATION


load :: IO (Maybe Config)
load = do
  configFile <- getConfigFile
  exists     <- D.doesFileExist configFile
  if not exists then
    pure Nothing
  else
   Json.decodeFileStrict configFile


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
