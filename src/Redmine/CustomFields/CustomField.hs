{-# LANGUAGE OverloadedStrings #-}
module Redmine.CustomFields.CustomField
  ( CustomField (..)
  ) where


import           Data.Aeson
import qualified Data.Text  as T



-- CUSTOM FIELD


data CustomField
  = CustomField
  { id             :: Int
  , name           :: T.Text
  , isRequired     :: Bool
  , fieldFormat    :: T.Text
  , customizedType :: T.Text
  } deriving (Show)



-- SERIALIZATION


instance FromJSON CustomField where
  parseJSON = withObject "CustomField" $ \v ->
    CustomField
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "is_required"
      <*> v .: "field_format"
      <*> v .: "customizedType"
