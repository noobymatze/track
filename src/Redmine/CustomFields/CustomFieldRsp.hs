{-# LANGUAGE DeriveGeneric #-}
module Redmine.CustomFields.CustomFieldRsp
  ( CustomFieldRsp (..)
  ) where


import           Data.Aeson
import           GHC.Generics
import qualified Redmine.CustomFields.CustomField as CustomField



-- CUSTOM FIELDS


newtype CustomFieldRsp
  = CustomFieldRsp
    { custom_fields :: [CustomField.CustomField]
    } deriving (Generic)



-- SERIALIZATION

instance FromJSON CustomFieldRsp where


