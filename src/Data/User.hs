{-# LANGUAGE DeriveGeneric #-}
module Data.User
  ( User (..)
  ) where


import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics



-- TIME ENTRY


data User
  = User
  { id    :: Int
  , login :: T.Text
  } deriving (Generic)



-- SERIALIZATION


instance FromJSON User where
instance ToJSON User where
