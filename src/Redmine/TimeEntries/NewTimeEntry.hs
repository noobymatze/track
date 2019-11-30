{-# LANGUAGE OverloadedStrings #-}
module Redmine.TimeEntries.NewTimeEntry
  ( NewTimeEntry (..)
  ) where


import           Data.Aeson
import qualified Data.Text                        as T
import qualified Data.Time                        as Time
import qualified Redmine.CustomFields.CustomValue as Custom



-- NEW TIME ENTRY


data NewTimeEntry
  = NewTimeEntry
  { newIssueId      :: Maybe Int
  , newProjectId    :: Maybe Int
  , newSpentOn      :: Maybe Time.Day
  , newHours        :: Double
  , newActivity     :: Int
  , newComments     :: Maybe T.Text
  , newCustomFields :: [Custom.CustomValue]
  } deriving (Show)



-- SERIALIZATION


instance ToJSON NewTimeEntry where
  toJSON entry =
    object
    [ "time_entry" .=
      object
      [ "issue_id"      .= newIssueId entry
      , "project_id"    .= newProjectId entry
      , "spent_on"      .= newSpentOn entry
      , "hours"         .= newHours entry
      , "activity"      .= newActivity entry
      , "comments"      .= newComments entry
      , "custom_fields" .= newCustomFields entry
      ]
    ]
