{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Redmine.TimeEntries.Activity
  ( Activity (..)
  , prompt
  ) where


import           Control.Applicative ((<|>))
import           Data.Aeson
import qualified Data.Maybe          as Maybe
import qualified Data.Text           as T
import           GHC.Generics
import           Helper              ((|>))
import qualified Prompt



-- TIME ENTRY ACTIVITIES


data Activity
  = Activity
  { aId       :: Int
  , name      :: T.Text
  , isDefault :: Bool
  } deriving (Generic)



-- PUBLIC HELPERS


prompt :: [Activity] -> Prompt.Prompt (Maybe Int)
prompt activities =
  let
    selected =
      activities
        |> filter isDefault
        |> Maybe.listToMaybe

    values =
      activities
        |> fmap (\a -> (aId a, name a))
  in
    Prompt.oneOf values (fmap aId selected)



-- SERIALIZATION


instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \v ->
    Activity
      <$> v .: "id"
      <*> v .: "name"
      <*> (v .: "is_default" <|> pure False)
