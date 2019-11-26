{-# LANGUAGE DeriveGeneric #-}
module Data.TimeEntryResult
  ( TimeEntryResult (..)
  ) where



import           Data.Aeson
import qualified Data.TimeEntry as TimeEntry
import           GHC.Generics



-- TIME ENTRY RESULT


data TimeEntryResult
  = TimeEntryResult
    { time_entries :: [TimeEntry.TimeEntry]
    } deriving (Generic)



-- SERIALIZATION

instance FromJSON TimeEntryResult where
