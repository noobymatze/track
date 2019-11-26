{-# LANGUAGE DeriveGeneric #-}
module Data.UsersResult
  ( UsersResult (..)
  ) where



import           Data.Aeson
import qualified Data.User    as User
import           GHC.Generics



-- TIME ENTRY RESULT


data UsersResult
  = UsersResult
    { users :: [User.User]
    } deriving (Generic)



-- SERIALIZATION

instance FromJSON UsersResult where
