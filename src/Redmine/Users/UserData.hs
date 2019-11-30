{-# LANGUAGE DeriveGeneric #-}
module Redmine.Users.UserData
  ( UserData (..)
  ) where


import           Data.Aeson
import           GHC.Generics
import qualified Redmine.Users.User as User



-- USER DATA


newtype UserData
  = UserData
  { user :: User.User
  } deriving (Show, Generic)



-- SERIALIZATION


instance FromJSON UserData where
