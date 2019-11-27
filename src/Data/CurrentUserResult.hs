{-# LANGUAGE DeriveGeneric #-}
module Data.CurrentUserResult
  ( CurrentUserResult (..)
  ) where



import           Data.Aeson
import qualified Data.User    as User
import           GHC.Generics



-- TIME ENTRY RESULT


data CurrentUserResult
  = CurrentUserResult
    { user :: User.User
    } deriving (Generic)



-- SERIALIZATION

instance FromJSON CurrentUserResult where
