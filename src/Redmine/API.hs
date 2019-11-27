{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Redmine.API
  ( RedmineAPI
  , redmine
  ) where


import qualified Data.CurrentUserResult as CurrentUserResult
import           Data.Proxy
import qualified Data.Text              as T
import qualified Data.TimeEntry         as TimeEntry
import qualified Data.TimeEntryResult   as TimeEntryResult
import           Servant.API



-- API


type RedmineAPI =
    UserAPI :<|> TimeEntryAPI

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
      :> QueryParam "user_id" Int
      :> ReqBody '[JSON] TimeEntry.TimeEntry
      :> Post '[JSON] ()



-- PROXY


redmine :: Proxy RedmineAPI
redmine =
  Proxy

