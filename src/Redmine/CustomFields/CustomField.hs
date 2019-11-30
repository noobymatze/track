{-# LANGUAGE OverloadedStrings #-}
module Redmine.CustomFields.CustomField
  ( CustomField (..)
  , isForTimeEntry
  , prompt
  ) where


import           Control.Applicative              ((<|>))
import           Data.Aeson
import qualified Data.Text                        as T
import           Prelude                          hiding (id)
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


prompt :: CustomField -> IO (Maybe CustomValue.CustomValue)
prompt field =
  case fieldFormat field of
    "bool" -> do
      putStr $ T.unpack $ name field
      putStr " [Y/n]: "
      Just . CustomValue.bool (id field) <$> promptYesOrNo

    "string" -> do
      putStr $ T.unpack $ name field
      putStr ": "
      maybeValue <- promptString (isRequired field)
      pure (CustomValue.string (id field) <$> maybeValue)

    format ->
      error $ "The type '" ++ T.unpack format ++ "' has not been implemented yet."


promptYesOrNo :: IO Bool
promptYesOrNo = getLine >>= \value ->
  if value `elem` ["y", "Y"] then
    pure True
  else if value `elem` ["n", "N"] then
    pure False
  else do
    putStr "Must type 'y' for yes or 'n' for no: "
    promptYesOrNo


promptString :: Bool -> IO (Maybe T.Text)
promptString required = (T.strip . T.pack) <$> getLine >>= \value ->
  if T.length value > 0 then
    pure (Just value)
  else if not required then
    pure Nothing
  else do
    putStr "Must insert something: "
    promptString required



-- SERIALIZATION


instance FromJSON CustomField where
  parseJSON = withObject "CustomField" $ \v ->
    CustomField
      <$> v .: "id"
      <*> v .: "name"
      <*> (v .: "is_required" <|> pure False)
      <*> v .: "field_format"
      <*> v .: "customized_type"
