{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Redmine.API
  ( RedmineAPI
  , redmine
  ) where


import           Data.Proxy
import qualified Data.Text            as T
import qualified Data.TimeEntry       as TimeEntry
import qualified Data.TimeEntryResult as TimeEntryResult
import qualified Data.UsersResult     as UsersResult
import           Servant.API



-- API


type RedmineAPI =
    UserAPI :<|> TimeEntryAPI

type UserAPI =
    "users.json"
      :> QueryParam "key" T.Text
      :> QueryParam "name" T.Text
      :> Get '[JSON] UsersResult.UsersResult


type TimeEntryAPI =
    "time_entries.json"
      :> QueryParam "key" T.Text
      :> QueryParam "spent_on" T.Text
      :> QueryParam "user_id" Int
      :> Get '[JSON] TimeEntryResult.TimeEntryResult
    :<|> "time_entries.json"
      :> QueryParam "key" T.Text
      :> QueryParam "user_id" Int
      :> ReqBody '[JSON] TimeEntry.TimeEntry
      :> Post '[JSON] ()



-- PROXY


redmine :: Proxy RedmineAPI
redmine =
  Proxy

