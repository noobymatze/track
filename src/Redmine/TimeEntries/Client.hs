module Redmine.TimeEntries.Client
  ( find
  , create
  ) where


import qualified App
import qualified Data.Text                         as T
import qualified Data.Time                         as Time
import qualified Redmine.TimeEntries.API           as API
import qualified Redmine.TimeEntries.LimitedResult as TimeEntries
import qualified Redmine.TimeEntries.NewTimeEntry  as TimeEntries
import qualified Redmine.TimeEntries.TimeEntry     as TimeEntries
import           Servant.API



-- WORK WITH TIME ENTRIES


find :: Time.Day -> Int -> App.App [TimeEntries.TimeEntry]
find spentOn userId = do
  key <- App.getApiKey
  TimeEntries.timeEntries <$> find_ key (formatDay spentOn) userId


create :: TimeEntries.NewTimeEntry -> App.App ()
create newEntry =
  App.getApiKey >>= flip create_ newEntry >> pure ()



-- RAW CLIENTS

find_ :: T.Text -> T.Text -> Int -> App.App TimeEntries.LimitedResult

create_ :: T.Text -> TimeEntries.NewTimeEntry -> App.App NoContent

(find_ :<|> create_) = App.client API.proxy



-- HELPERS


formatDay :: Time.Day -> T.Text
formatDay =
  T.pack . Time.formatTime Time.defaultTimeLocale "%F"

