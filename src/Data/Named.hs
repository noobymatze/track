{-# LANGUAGE DeriveGeneric #-}
module Data.Named
  ( Named (..)
  ) where


import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics



-- USER


data Named
  = Named
    { id   :: Int
    , name :: T.Text
    } deriving (Show, Generic)



-- SERIALIZATION


instance FromJSON Named where
instance ToJSON Named where
