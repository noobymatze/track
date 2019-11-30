{-# LANGUAGE DeriveGeneric #-}
module Redmine.Users.User
  ( User (..)
  ) where


import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics



-- USER


data User
  = User
  { id    :: Int
  , login :: T.Text
  } deriving (Show, Generic)



-- SERIALIZATION


instance FromJSON User where
