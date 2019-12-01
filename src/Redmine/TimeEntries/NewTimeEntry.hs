{-# LANGUAGE OverloadedStrings #-}
module Redmine.TimeEntries.NewTimeEntry
  ( NewTimeEntry (..)
  , prompt
  ) where


import           Data.Aeson
import qualified Data.Maybe                       as Maybe
import qualified Data.Text                        as T
import qualified Data.Time                        as Time
import           Helper                           ((|>))
import qualified Prompt
import qualified Redmine.CustomFields.CustomField as CustomField
import qualified Redmine.CustomFields.CustomValue as Custom



-- NEW TIME ENTRY


data NewTimeEntry
  = NewTimeEntry
  { newIssueId      :: Maybe Int
  , newProjectId    :: Maybe Int
  , newSpentOn      :: Time.Day
  , newHours        :: Double
  , newActivity     :: Int
  , newComments     :: Maybe T.Text
  , newCustomValues :: [Custom.CustomValue]
  } deriving (Show)



-- PUBLIC HELPERS


prompt :: Time.Day -> [CustomField.CustomField] -> Prompt.Prompt NewTimeEntry
prompt today customFields =
  let
    issueId =
      Prompt.int
        |> Prompt.label "Issue: "

    projectId =
      Prompt.int
        |> Prompt.label "Project: "

    hours =
      Prompt.double
        |> Prompt.label "Time spent: "
        |> Prompt.required (Just "Must insert your time spent: ")

    activity =
      Prompt.int
        |> Prompt.label "Activity: "
        |> Prompt.required (Just "Must insert the activity: ")

    comment =
      Prompt.string
        |> Prompt.label "Message: "

    customValues =
      customFields
        |> traverse CustomField.prompt
        |> fmap Maybe.catMaybes
  in do
    issue <- issueId
    project <- projectId
    m <- comment
    NewTimeEntry
      <$> pure issue
      <*> pure project
      <*> pure today
      <*> hours
      <*> activity
      <*> pure m
      <*> customValues



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
      , "custom_fields" .= newCustomValues entry
      ]
    ]
