{-# LANGUAGE OverloadedStrings #-}
module Redmine.TimeEntries.LimitedResult
  ( LimitedResult (..)
  ) where


import           Data.Aeson
import qualified Redmine.TimeEntries.TimeEntry as TimeEntry



-- LIMITED RESULT


data LimitedResult
  = LimitedResult
    { timeEntries :: [TimeEntry.TimeEntry]
    , offset      :: Int
    , limit       :: Int
    , totalCount  :: Int
    }



-- SERIALIZATION


instance FromJSON LimitedResult where
  parseJSON = withObject "LimitedResult" $ \v ->
    LimitedResult
      <$> v .: "time_entries"
      <*> v .: "offset"
      <*> v .: "limit"
      <*> v .: "total_count"
