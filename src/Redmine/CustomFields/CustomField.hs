{-# LANGUAGE OverloadedStrings #-}
module Redmine.CustomFields.CustomField
  ( CustomField (..)
  , isForTimeEntry
  , prompt
  ) where


import           Control.Applicative              ((<|>))
import           Data.Aeson
import           Data.Semigroup                   ((<>))
import qualified Data.Text                        as T
import           Helper                           ((|>))
import           Prelude                          hiding (id)
import qualified Prompt
import qualified Redmine.CustomFields.CustomValue as CustomValue



-- CUSTOM FIELD


data CustomField
  = CustomField
  { id             :: Int
  , name           :: T.Text
  , isRequired     :: Bool
  , fieldFormat    :: T.Text
  , customizedType :: T.Text
  } deriving (Show)



-- PUBLIC HELPERS


isForTimeEntry :: CustomField -> Bool
isForTimeEntry field =
  customizedType field == "time_entry"


prompt :: CustomField -> Prompt.Prompt (Maybe CustomValue.CustomValue)
prompt field =
  case fieldFormat field of
    "bool" ->
      Prompt.yesOrNo
        |> Prompt.label (name field <> " [Y/n]: ")
        |> Prompt.required (Just "Must type 'y' for yes or 'n' for no: ")
        |> fmap (Just . CustomValue.bool (id field))

    "string" ->
      Prompt.string
        |> Prompt.label (name field <> ": ")
        |> requiredMaybe (isRequired field) "This is required: "
        |> fmap (fmap (CustomValue.string (id field)))

    format ->
      error $ "The type '" ++ T.unpack format ++ "' has not been implemented yet."



-- HELPERS


requiredMaybe :: Bool -> T.Text -> Prompt.Prompt (Maybe a) -> Prompt.Prompt (Maybe a)
requiredMaybe required requiredLabel p =
  if required then
    Just <$> Prompt.required' requiredLabel p
  else
    p



-- SERIALIZATION


instance FromJSON CustomField where
  parseJSON = withObject "CustomField" $ \v ->
    CustomField
      <$> v .: "id"
      <*> v .: "name"
      <*> (v .: "is_required" <|> pure False)
      <*> v .: "field_format"
      <*> v .: "customized_type"
