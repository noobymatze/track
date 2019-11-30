{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Redmine.Users.API
  ( API
  , proxy
  ) where


import           Data.Proxy             (Proxy (..))
import qualified Data.Text              as T
import qualified Redmine.Users.UserData as Users
import           Servant.API



-- API


type API =
    "users"
      :> "current.json"
      :> QueryParam' '[Required] "key" T.Text
      :> Get '[JSON] Users.UserData



-- PROXY


proxy :: Proxy API
proxy =
  Proxy
