{-# LANGUAGE OverloadedStrings #-}
module Redmine.TimeEntries.Client
  ( find
  , create
  , getActivities
  ) where


import qualified App
import qualified Data.Text                         as T
import qualified Data.Time                         as Time
import qualified Redmine.TimeEntries.Activities    as TimeEntries
import qualified Redmine.TimeEntries.Activity      as TimeEntries
import qualified Redmine.TimeEntries.API           as API
import qualified Redmine.TimeEntries.LimitedResult as TimeEntries
import qualified Redmine.TimeEntries.NewTimeEntry  as TimeEntries
import qualified Redmine.TimeEntries.TimeEntry     as TimeEntries
import           Servant.API



-- WORK WITH TIME ENTRIES


find :: Time.Day -> Int -> App.App [TimeEntries.TimeEntry]
find spentOn userId = do
  key <- App.getApiKey
  TimeEntries.timeEntries <$> find_ key (formatDay spentOn) userId (Just "spent_on:asc")


create :: TimeEntries.NewTimeEntry -> App.App ()
create newEntry =
  App.getApiKey >>= flip create_ newEntry >> pure ()


getActivities :: App.App [TimeEntries.Activity]
getActivities =
  TimeEntries.activities <$> (App.getApiKey >>= getActivities_)



-- RAW CLIENTS

find_ :: T.Text -> T.Text -> Int -> Maybe T.Text -> App.App TimeEntries.LimitedResult

create_ :: T.Text -> TimeEntries.NewTimeEntry -> App.App NoContent

getActivities_ :: T.Text -> App.App TimeEntries.Activities

(find_ :<|> create_ :<|> getActivities_) = App.client API.proxy



-- HELPERS


formatDay :: Time.Day -> T.Text
formatDay =
  T.pack . Time.formatTime Time.defaultTimeLocale "%F"

