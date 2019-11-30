{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Redmine.CustomFields.CustomValue
  ( CustomValue (..)
  , bool
  , string
  ) where


import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics



-- CUSTOM VALUE


data CustomValue
  = CustomValue
    { id    :: Int
    , value :: T.Text
    } deriving (Show, Generic)



-- PUBLIC HELPERS


bool :: Int -> Bool -> CustomValue
bool identifier yes =
  CustomValue identifier $
    if yes then
      "1"
    else
      "0"


string :: Int -> T.Text -> CustomValue
string =
  CustomValue


-- SERIALIZATION


instance ToJSON CustomValue where
