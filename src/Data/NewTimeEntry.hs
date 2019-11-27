{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.NewTimeEntry
  ( NewTimeEntry (..)
  ) where


import           Data.Aeson
import qualified Data.Custom     as Custom
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import qualified Data.Time       as Time
import           GHC.Generics
import           Helper          ((|>))



-- NEW TIME ENTRY


data NewTimeEntry
  = NewTimeEntry
  { issue_id      :: Maybe Int
  , project_id    :: Maybe Int
  , spent_on      :: Maybe Time.Day
  , hours         :: Double
  , activity      :: Int
  , comments      :: Maybe T.Text
  , custom_fields :: [Custom.CustomValue]
  } deriving (Show, Generic)



-- SERIALIZATION


instance ToJSON NewTimeEntry where
  toJSON entry =
    object
    [ "time_entry" .=
      object
      [ "issue_id"      .= issue_id entry
      , "project_id"    .= project_id entry
      , "spent_on"      .= spent_on entry
      , "hours"         .= hours entry
      , "activity"      .= activity entry
      , "comments"      .= comments entry
      , "custom_fields" .= custom_fields entry
      ]
    ]
