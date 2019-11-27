module Redmine.Client
  ( getEntries
  , createEntry
  , getCurrentUser
  , getCustomFields
  ) where

import qualified Data.CurrentUserResult as CurrentUserResult
import qualified Data.Custom            as Custom
import qualified Data.CustomFields      as CustomFields
import qualified Data.NewTimeEntry      as NewTimeEntry
import qualified Data.Text              as T
import qualified Data.Time              as Time
import qualified Data.TimeEntry         as TimeEntry
import qualified Data.TimeEntryResult   as TimeEntryResult
import qualified Data.User              as User
import           Helper                 ((|>))
import qualified Redmine.API            as API
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


createEntry :: T.Text -> NewTimeEntry.NewTimeEntry -> ClientM ()
createEntry key =
  createEntry_ (Just key)



-- USERS


getCurrentUser :: T.Text -> ClientM User.User
getCurrentUser key =
  key
    |> Just
    |> getUser_
    |> fmap CurrentUserResult.user



-- CUSTOM FIELDS


getCustomFields :: T.Text -> ClientM [Custom.Field]
getCustomFields key =
  key
    |> Just
    |> getCustomFields_
    |> fmap CustomFields.custom_fields




-- RAW GENERATED API


getEntries_ :: Maybe T.Text
            -> Maybe T.Text
            -> Maybe Int
            -> ClientM TimeEntryResult.TimeEntryResult

createEntry_ :: Maybe T.Text
             -> NewTimeEntry.NewTimeEntry
             -> ClientM ()

getUser_ :: Maybe T.Text
         -> ClientM CurrentUserResult.CurrentUserResult

getCustomFields_ :: Maybe T.Text
                 -> ClientM CustomFields.CustomFields

(getUser_ :<|> (getEntries_ :<|> createEntry_) :<|> getCustomFields_) = client API.redmine
