{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Custom
  ( Field (..)
  , CustomValue (..)
  , parser
  ) where


import           Data.Aeson
import qualified Data.Text           as T
import           GHC.Generics
import           Options.Applicative



-- TIME ENTRY


data Field
  = Field
  { fieldId         :: Int
  , name            :: T.Text
  , is_required     :: Bool
  , field_format    :: T.Text
  , customized_type :: T.Text
  } deriving (Show, Generic)


data CustomValue
  = CustomValue
    { id    :: Int
    , value :: T.Text
    } deriving (Show, Generic)



-- PUBLIC HELPERS


parser :: Field -> Parser CustomValue
parser field =
  let
    boolToValue id True  = CustomValue id "1"
    boolToValue id False = CustomValue id "0"
  in
  case (is_required field, customized_type field, field_format field) of
    (True, "time_entry", "bool") ->
      boolToValue (fieldId field) <$> switch
      ( long (T.unpack (T.toLower (name field)))
      <> help "Custom field "
      )









-- SERIALIZATION


instance FromJSON Field where
instance ToJSON CustomValue where
