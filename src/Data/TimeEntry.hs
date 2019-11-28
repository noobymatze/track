{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.TimeEntry
  ( TimeEntry (..)
  , display
  ) where


import           Data.Aeson
import qualified Data.Named   as Named
import qualified Data.Text    as T
import           GHC.Generics



-- TIME ENTRY


data TimeEntry
  = TimeEntry
  { id       :: Int
  , user     :: Named.Named
  , project  :: Named.Named
  , hours    :: Double
  , comments :: T.Text
  } deriving (Generic)



-- PUBLIC HELPERS


display :: TimeEntry -> T.Text
display entry =
  T.intercalate " "
  [ T.pack (show (hours entry))
  , Named.name (project entry)
  , comments entry
  ]



-- SERIALIZATION


instance FromJSON TimeEntry where
instance ToJSON TimeEntry where
