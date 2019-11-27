{-# LANGUAGE DeriveGeneric #-}
module Data.CustomFields
  ( CustomFields (..)
  ) where



import           Data.Aeson
import qualified Data.Custom  as Custom
import           GHC.Generics



-- TIME ENTRY RESULT


data CustomFields
  = CustomFields
    { custom_fields :: [Custom.Field]
    } deriving (Generic)



-- SERIALIZATION

instance FromJSON CustomFields where
