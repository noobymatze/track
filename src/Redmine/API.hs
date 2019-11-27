{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Redmine.API
  ( RedmineAPI
  , redmine
  ) where


import qualified Data.CurrentUserResult as CurrentUserResult
import qualified Data.CustomFields      as CustomFields
import qualified Data.NewTimeEntry      as NewTimeEntry
import           Data.Proxy
import qualified Data.Text              as T
import qualified Data.TimeEntryResult   as TimeEntryResult
import           Servant.API



-- API


type RedmineAPI =
    UserAPI :<|> TimeEntryAPI :<|> CustomFieldsAPI


type UserAPI =
    "users"
      :> "current.json"
      :> QueryParam "key" T.Text
      :> Get '[JSON] CurrentUserResult.CurrentUserResult


type TimeEntryAPI =
    "time_entries.json"
      :> QueryParam "key" T.Text
      :> QueryParam "spent_on" T.Text
      :> QueryParam "user_id" Int
      :> Get '[JSON] TimeEntryResult.TimeEntryResult
    :<|> "time_entries.json"
      :> QueryParam "key" T.Text
      :> ReqBody '[JSON] NewTimeEntry.NewTimeEntry
      :> Post '[JSON] ()


type CustomFieldsAPI =
    "custom_fields.json"
      :> QueryParam "key" T.Text
      :> Get '[JSON] CustomFields.CustomFields


-- PROXY


redmine :: Proxy RedmineAPI
redmine =
  Proxy

