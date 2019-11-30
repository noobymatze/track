{-# LANGUAGE DeriveGeneric #-}
module Redmine.CustomFields.CustomValue
  ( CustomValue (..)
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



-- SERIALIZATION


instance ToJSON CustomValue where
