module Redmine.Client
  ( getEntries
  , createEntry
  , getUser
  ) where

import qualified Data.Text            as T
import qualified Data.Time            as Time
import qualified Data.TimeEntry       as TimeEntry
import qualified Data.TimeEntryResult as TimeEntryResult
import qualified Data.User            as User
import qualified Data.UsersResult     as UsersResult
import           Helper               ((|>))
import qualified Redmine.API          as API
import           Servant.API
import           Servant.Client


-- WORK WITH TIME ENTRIES


getEntries :: T.Text -> Time.Day -> Int -> ClientM [TimeEntry.TimeEntry]
getEntries key spentOnDay userId =
  let
    spentOn =
      spentOnDay
        |> Time.formatTime Time.defaultTimeLocale "%F"
        |> T.pack
  in
    getEntries_ (Just key) (Just spentOn) (Just userId)
      |> fmap TimeEntryResult.time_entries


createEntry :: T.Text -> Int -> TimeEntry.TimeEntry -> ClientM ()
createEntry key userId =
  createEntry_ (Just key) (Just userId)


getUser :: T.Text -> T.Text -> ClientM (Maybe User.User)
getUser key name = do
  users <- fmap UsersResult.users (getUsers_ (Just key) (Just name))
  case users of
    [user] ->
      pure (Just user)

    _ ->
      pure Nothing






-- RAW GENERATED API


getEntries_ :: Maybe T.Text
            -> Maybe T.Text
            -> Maybe Int
            -> ClientM TimeEntryResult.TimeEntryResult

createEntry_ :: Maybe T.Text
             -> Maybe Int
             -> TimeEntry.TimeEntry
             -> ClientM ()

getUsers_ :: Maybe T.Text
         -> Maybe T.Text
         -> ClientM UsersResult.UsersResult

(getUsers_ :<|> getEntries_ :<|> createEntry_) = client API.redmine
