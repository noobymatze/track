{-# LANGUAGE OverloadedStrings #-}
module Redmine.TimeEntries.NewTimeEntry
  ( NewTimeEntry (..)
  , prompt
  ) where


import           Control.Monad.IO.Class                   (liftIO)
import           Data.Aeson
import qualified Data.Maybe                               as Maybe
import qualified Data.Text                                as T
import qualified Data.Time                                as Time
import           Helper                                   ((|>))
import qualified Prompt
import qualified Redmine.CustomFields.CustomField         as CustomField
import qualified Redmine.CustomFields.CustomValue         as Custom
import qualified Redmine.TimeEntries.NewTimeEntry.Comment as Comment



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


prompt :: Time.Day
       -> [CustomField.CustomField]
       -> Prompt.Prompt NewTimeEntry
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
        |> Prompt.required "Must insert your time spent: "

    activity =
      Prompt.int
        |> Prompt.label "Activity: "
        |> Prompt.required "Must insert the activity: "

    comment =
      Prompt.string
        |> Prompt.label "Message: "

    customValues =
      customFields
        |> traverse CustomField.prompt
        |> fmap Maybe.catMaybes

    parseComment message =
      case fmap Comment.parse message of
        Just (Right c) ->
          (Comment.getIssue c, Comment.getTimeSpent c, message)

        _ ->
          (Nothing, Nothing, message)
  in do
    issue <- issueId
    project <- promptForProject issue projectId
    (_, timeSpent, m) <- parseComment <$> comment
    liftIO $ printTimeSpent timeSpent
    NewTimeEntry
      <$> pure issue
      <*> pure project
      <*> pure today
      <*> maybe hours pure timeSpent
      <*> activity
      <*> pure m
      <*> customValues


promptForProject :: Maybe Int -> Prompt.Prompt (Maybe Int) -> Prompt.Prompt (Maybe Int)
promptForProject maybeIssueId projectId =
  case maybeIssueId of
    Nothing ->
        projectId
          |> Prompt.required "Please insert the project number: "
          |> fmap Just

    Just _ ->
      pure Nothing


printTimeSpent :: Maybe Double -> IO ()
printTimeSpent maybeTimeSpent =
  case maybeTimeSpent of
    Nothing ->
      pure ()

    Just timeSpent ->
      putStrLn $ "Time spent: " <> show timeSpent



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
