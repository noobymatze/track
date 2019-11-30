module Redmine.Users.Client
  ( getCurrent
  ) where


import qualified App
import qualified Data.Text              as T
import qualified Redmine.Users.API      as API
import qualified Redmine.Users.User     as User
import qualified Redmine.Users.UserData as UserData



-- WORK WITH USERS


getCurrent :: App.App User.User
getCurrent =
  UserData.user <$> (App.getApiKey >>= getCurrent_)


getCurrent_ :: T.Text -> App.App UserData.UserData
getCurrent_ = App.client API.proxy
