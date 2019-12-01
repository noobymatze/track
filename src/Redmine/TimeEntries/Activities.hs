{-# LANGUAGE OverloadedStrings #-}
module Redmine.TimeEntries.Activities
  ( Activities (..)
  ) where


import           Data.Aeson
import qualified Redmine.TimeEntries.Activity as Activity



-- LIMITED RESULT


newtype Activities
  = Activities
    { activities :: [Activity.Activity]
    }



-- SERIALIZATION


instance FromJSON Activities where
  parseJSON = withObject "Activities" $ \v ->
    Activities
      <$> v .: "time_entry_activities"
