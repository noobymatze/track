module Redmine.Client
  ( getEntries
  , createEntry
  , getCurrentUser
  , getCustomFields
  ) where


import qualified App
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



-- WORK WITH TIME ENTRIES


getEntries :: Time.Day -> Int -> App.App [TimeEntry.TimeEntry]
getEntries spentOnDay userId =
  let
    spentOn =
      spentOnDay
        |> Time.formatTime Time.defaultTimeLocale "%F"
        |> T.pack
  in do
    apiKey <- App.getApiKey
    getEntries_ (Just apiKey) (Just spentOn) (Just userId)
      |> fmap TimeEntryResult.time_entries


createEntry :: NewTimeEntry.NewTimeEntry -> App.App ()
createEntry newEntry = do
  apiKey <- App.getApiKey
  createEntry_ (Just apiKey) newEntry



-- USERS


getCurrentUser :: App.App User.User
getCurrentUser = do
  apiKey <- App.getApiKey
  CurrentUserResult.user <$> getUser_ (Just apiKey)



-- CUSTOM FIELDS


getCustomFields :: App.App [Custom.Field]
getCustomFields = do
  apiKey <- App.getApiKey
  CustomFields.custom_fields <$> getCustomFields_ (Just apiKey)



-- RAW GENERATED API


getEntries_ :: Maybe T.Text
            -> Maybe T.Text
            -> Maybe Int
            -> App.App TimeEntryResult.TimeEntryResult

createEntry_ :: Maybe T.Text
             -> NewTimeEntry.NewTimeEntry
             -> App.App ()

getUser_ :: Maybe T.Text
         -> App.App CurrentUserResult.CurrentUserResult

getCustomFields_ :: Maybe T.Text
                 -> App.App CustomFields.CustomFields

(getUser_ :<|> (getEntries_ :<|> createEntry_) :<|> getCustomFields_) = App.client API.redmine
